{-# LANGUAGE CPP #-}

module Z80.MisosysEDAS.Assembler 
  ( -- * Types and Data Constructors
    EDASAsmOutput

    -- * Functions
  , edasAssemble
  , evalAsmExpr
  , evalAsmExprWord8
  ) where

-- import Debug.Trace

#ifdef mingw32_HOST_OS
import Control.Lens hiding (value, op)
#else
import Control.Lens hiding (value)
#endif

import Prelude hiding (words)
import Control.Monad
import Data.Either
import Data.Word
import Data.Int
import Data.List hiding (words)
import Data.Bits
import Data.Time
import System.Locale
-- import Data.Time.Format
import Text.Parsec.Pos
import qualified Data.Char as C
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as DVU

import Machine.Utils
import Z80.Processor
import Z80.MisosysEDAS.Types

-- | Shorthand for catching intermediate assembler issues and problems
type IntermediateCtx = Either T.Text AsmEvalCtx

-- | Shorthand for the assembler's final result
type EDASAsmOutput = IO (Either T.Text (AsmEvalCtx, [AsmStmt]))

-- | The assembler pass: Evaluate the pseudo-operations and expressions within instructions, update the assembler
-- statement with the results and 'Z80instruction's.
--
-- N.B.: The result is embedded in the 'IO' monad due to the need to call 'Data.Time.getCurrentTime', which returns
-- 'IO' 'UTCTime'. Yuck.
edasAssemble :: Either T.Text [AsmStmt]                 -- ^ Parser result
             -> EDASAsmOutput                           -- ^ Assembler result
edasAssemble parseResult =
  case parseResult of
    Left stuff   -> return $ Left stuff
    Right  stmts -> getZonedTime
                    >>= (\currentTime ->
                          let initialCtx = Right $ AsmEvalCtx { _equateTab = Map.empty
                                                              , _labelTab  = Map.empty
                                                              , _asmPC     = 0
                                                              , _dateTime  = currentTime
                                                              }
                              (finalctx, result) = mapAccumL evalAsmStmt initialCtx stmts
                          in  return $ case finalctx of
                                         Left stuff   -> Left stuff
                                         Right  ctx   -> Right (ctx, result)
                        )

-- | Evaluate a single assembler statement
evalAsmStmt :: IntermediateCtx
            -> AsmStmt
            -> (IntermediateCtx, AsmStmt)
evalAsmStmt ictx stmt =
  case ictx of
    Left stuff  -> (Left stuff, stmt)
    Right ctx   ->
      let currentPC = ctx ^. asmPC
          -- Associate the statement's label with the current program counter
          stmtCtx   = case stmt ^. symLabel of
                        Nothing    -> ctx
                        Just label -> insertSymLabel ctx label currentPC
      in  case stmt ^. asmOp of
            Nothing              -> ( Right stmtCtx
                                    , stmtAddr .~ currentPC $ stmt
                                    )
            Just (Insn _insn)    -> (Right stmtCtx, stmt)
            Just (Pseudo pseudo) -> evalPseudo stmtCtx stmt pseudo

evalPseudo :: AsmEvalCtx
           -> AsmStmt
           -> EDASPseudo
           -> (IntermediateCtx, AsmStmt)
evalPseudo ctx stmt pseudo =
    case pseudo of
      Equate expr    -> (evalEquate (stmt ^. symLabel) expr ctx, stmt)
      Origin org     -> (liftM (\o -> asmPC .~ o $ ctx) (evalAsmExpr ctx org), stmt)
      DefB args      -> let cvtDBValue (DBStr str)   = Right $ map charIntegral $ T.unpack str
                            cvtDBValue (DBExpr expr) = liftM (: []) (evalAsmExprWord8 ctx expr)
                        in  evalDefXXArgs args cvtDBValue ctx stmt
      DefC rept fill -> evalDefC rept fill ctx stmt
      DefS rept      -> evalDefS rept ctx stmt
      DefW args      -> let cvtDWValue (DWChars c1 c2) = Right $ [(charIntegral c1), (charIntegral c2)]
                            cvtDWValue (DWExpr expr)   = liftM word2ByteList (evalAsmExpr ctx expr)
                        in  evalDefXXArgs args cvtDWValue ctx stmt
      DSym sym       -> emitText sym ctx stmt
      DExp expr      -> case liftM word2ByteList (evalAsmExpr ctx expr) of
                          Left issues -> (Left issues, stmt)
                          Right words -> let currentPC = ctx ^. asmPC
                                             theBytes  = DVU.fromList words
                                         in ( Right $ asmPC %~ (+ (fromIntegral . DVU.length) theBytes) $ ctx
                                            , stmtAddr .~ currentPC $ bytes .~ theBytes $ stmt
                                            )
      AsmDate        -> emitString (ctx ^. dateTime & formatTime defaultTimeLocale "%D") ctx stmt
      AsmTime        -> emitString (ctx ^. dateTime & formatTime defaultTimeLocale "%T") ctx stmt

-- | Evaluate a symbol equate
evalEquate :: Maybe EDASLabel
           -> EDASExpr
           -> AsmEvalCtx
           -> IntermediateCtx
evalEquate Nothing    _    _   = Left "Equate is missing symbol to which to assign result."
evalEquate (Just sym) expr ctx = liftM (insertEquate ctx sym) (evalAsmExpr ctx expr)

-- | Evaluate the "db"/"defb", "dw"/"defw" argument lists and fold the generated bytes into the assembler statement
evalDefXXArgs :: [argType]
              -> (argType -> Either T.Text [Z80word])
              -> AsmEvalCtx
              -> AsmStmt
              -> (IntermediateCtx, AsmStmt)
evalDefXXArgs args cvtFunc ctx stmt =
        let (ls, rs)                 = partitionEithers $ map cvtFunc args
            theBytes                 = DVU.concat $ map DVU.fromList rs
            currentPC                = ctx ^. asmPC
        in  if (not . null) ls then
              -- Only report the first error
              (Left $ head ls, stmt)
            else
              -- No error: move the program counter forward, save the bytes generated
              ( Right $ (asmPC %~ (+ (fromIntegral . DVU.length) theBytes)) $ ctx
              , stmtAddr .~ currentPC $ bytes .~ theBytes $ stmt
              )

-- | Define constant fill/block
evalDefC :: EDASExpr
         -> EDASExpr
         -> AsmEvalCtx
         -> AsmStmt
         -> (IntermediateCtx, AsmStmt)
evalDefC rept fill ctx stmt =
  let reptVal = evalAsmExpr ctx rept
      fillVal = evalAsmExprWord8 ctx fill
      currentPC = ctx ^. asmPC
  in  case reptVal of
        Left reptErr -> (Left reptErr, stmt)
        Right rval   -> case fillVal of
                          Left fillErr -> (Left fillErr, stmt)
                          Right fval   -> let theBytes = DVU.replicate (fromIntegral rval) fval
                                          in  ( Right $ (asmPC %~ (+ (fromIntegral . DVU.length) theBytes) $ ctx)
                                              , stmtAddr .~ currentPC $ bytes .~ theBytes $ stmt
                                              )

-- | Define aribtrary space
evalDefS :: EDASExpr
         -> AsmEvalCtx
         -> AsmStmt
         -> (IntermediateCtx, AsmStmt)
evalDefS rept ctx stmt =
  let reptVal = evalAsmExpr ctx rept
      currentPC = ctx ^. asmPC
  in  case reptVal of
        Left reptErr -> (Left reptErr, stmt)
        Right rval   -> let theBytes = DVU.replicate (fromIntegral rval) (0 :: Z80word)
                        in  ( Right $ (asmPC %~ (+ (fromIntegral . DVU.length) theBytes) $ ctx)
                            , stmtAddr .~ currentPC $ bytes .~ theBytes $ stmt
                            )

-- | Convert a string to bytes, update statement and intermediate assembler context.
emitString :: String
           -> AsmEvalCtx
           -> AsmStmt
           -> (IntermediateCtx, AsmStmt)
emitString str ctx stmt = let theBytes  = DVU.fromList (stringToWords str)
                              currentPC = ctx ^. asmPC
                          in  ( Right $ (asmPC %~ (+ (fromIntegral . DVU.length) theBytes)) $ ctx
                              , stmtAddr .~ currentPC $ bytes .~ theBytes $ stmt
                              )

-- | Emit 'Data.Text' string as a sequence of bytes
emitText :: T.Text
         -> AsmEvalCtx
         -> AsmStmt
         -> (IntermediateCtx, AsmStmt)
emitText = emitString . T.unpack

-- | Convert word to list of bytes in little endian order 
word2ByteList :: Word16
              -> [Z80word]
word2ByteList w = [(fromIntegral (w .&. 0xff)), (fromIntegral (w `shiftR` 8))]

-- | Evaluate an assembler expression to produce a 'Word16' result, within the current assembler evaluation context
evalAsmExpr :: AsmEvalCtx
            -> EDASExpr
            -> Either T.Text Word16
evalAsmExpr _ctx (Const _srcloc cst) = Right (fromIntegral cst)
evalAsmExpr  ctx (Var pos v)         = case findAsmSymbol ctx v of
                                         Nothing -> Left (T.concat [ mkSourcePosT pos
                                                                   , "Unknown equate or label name: "
                                                                   , v
                                                                   ]
                                                         )
                                         Just x  -> Right x
evalAsmExpr _ctx (AsmChar c)         = Right (charIntegral c)
evalAsmExpr ctx  (Add l r)           = evalBinOp ctx (+) l r
evalAsmExpr ctx  (Sub l r)           = evalBinOp ctx (-) l r
evalAsmExpr ctx  (Mul l r)           = evalBinOp ctx (*) l r
evalAsmExpr ctx  (Div l r)           = liftM2 (\l' r' -> l' `div` (r' .&. 0xff)) (evalAsmExpr ctx l) (evalAsmExpr ctx r)
evalAsmExpr ctx  (Mod l r)           = liftM2 (\l' r' -> l' `mod` (r' .&. 0xff)) (evalAsmExpr ctx l) (evalAsmExpr ctx r)
evalAsmExpr ctx  (Shift v x)         = liftM2 (\v' x' -> let x'' = (fromIntegral x') :: Int16
                                                         in  v' `shift` (fromIntegral x''))
                                              (evalAsmExpr ctx v)
                                              (evalAsmExpr ctx x)
evalAsmExpr ctx  (LogAnd l r)        = evalBinOp ctx (.&.) l r
evalAsmExpr ctx  (LogOr  l r)        = evalBinOp ctx (.|.) l r
evalAsmExpr ctx  (LogXor l r)        = evalBinOp ctx xor l r
evalAsmExpr ctx  (LogNE  l r)        = evalCompare ctx (/=) l r
evalAsmExpr ctx  (LogEQ  l r)        = evalCompare ctx (==) l r
evalAsmExpr ctx  (LogGE  l r)        = evalCompare ctx (>=) l r
evalAsmExpr ctx  (LogGT  l r)        = evalCompare ctx (>)  l r
evalAsmExpr ctx  (LogLE  l r)        = evalCompare ctx (<=) l r
evalAsmExpr ctx  (LogLT  l r)        = evalCompare ctx (<)  l r
evalAsmExpr ctx  (ShiftL v x)        = liftM2 (\v' x' -> v' `shiftL` (fromIntegral x')) (evalAsmExpr ctx v) (evalAsmExpr ctx x)
evalAsmExpr ctx  (ShiftR v x)        = liftM2 (\v' x' -> v' `shiftR` (fromIntegral x')) (evalAsmExpr ctx v) (evalAsmExpr ctx x)
evalAsmExpr ctx  (OnesCpl x)         = evalUnaryOp ctx complement x
evalAsmExpr ctx  (HighByte x)        = evalUnaryOp ctx (\y -> (y `shiftR` 8) .&. 0xff) x
evalAsmExpr ctx  (LowByte x)         = evalUnaryOp ctx (\y -> y .&. 0xff) x

-- | Evaluate a unary operator expression
evalUnaryOp :: AsmEvalCtx
            -> (Word16 -> Word16)
            -> EDASExpr
            -> Either T.Text Word16
evalUnaryOp ctx op x = liftM op (evalAsmExpr ctx x)

-- | Evaluate a binary operator expression
evalBinOp :: AsmEvalCtx
          -> (Word16 -> Word16 -> Word16)
          -> EDASExpr
          -> EDASExpr
          -> Either T.Text Word16
evalBinOp ctx op l r = liftM2 op (evalAsmExpr ctx l) (evalAsmExpr ctx r)

-- | Evaluate a comparison expression
evalCompare :: AsmEvalCtx
            -> (Word16 -> Word16 -> Bool)
            -> EDASExpr
            -> EDASExpr
            -> Either T.Text Word16
evalCompare ctx op l r = liftM2 compareResult (evalAsmExpr ctx l) (evalAsmExpr ctx r)
  where
    compareResult l' r' = if l' `op` r' then
                          0 :: Word16
                        else
                          0xffff :: Word16

-- | Evaluate an assembler expression to produce a 'Word8' result, within the current assembler evaluation context
evalAsmExprWord8 :: AsmEvalCtx
                 -> EDASExpr
                 -> Either T.Text Z80word
evalAsmExprWord8 ctx expr = rangeCheck (evalAsmExpr ctx expr)
  where
    rangeCheck (Left  x) = Left x
    rangeCheck (Right x) = if (x .&. 0xff00 == 0xff00) || (x <= 0xff) then
                             Right $ fromIntegral (x .&. 0xff)
                           else
                             Left (T.concat [ "value out of range for 8-bit value: "
                                            , (T.pack . show) x
                                            , "("
                                            , (as0xHex x)
                                            , ")"
                                            ]
                                  )

-- | Utility function for outputting the source position
mkSourcePosT :: SourcePos
             -> T.Text
mkSourcePosT srcpos = T.concat [ (T.pack . sourceName) srcpos
                               , ", line "
                               , (T.pack . show . sourceLine) srcpos
                               , ", col "
                               , (T.pack . show . sourceColumn) srcpos
                               , ": "
                               ]

-- | Convert 'Char' to an integral word type ('Word16' or 'Word8')
charIntegral :: (Integral wordType) => Char
          -> wordType
charIntegral = fromIntegral . C.ord

-- | Convert a 'String' to a list of integral words ('Word16' or 'Word8', generally)
stringToWords :: (Integral wordType) =>
                 String
              -> [wordType]
stringToWords = map charIntegral
