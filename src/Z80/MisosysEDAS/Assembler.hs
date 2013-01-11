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
import Data.Maybe
import Data.Word
import Data.Int
import Data.List hiding (words)
import Data.Bits
import Data.Time
import System.Locale
-- import Data.Time.Format
import Text.Parsec.Pos
import qualified Data.Char as C
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
edasAssemble :: Either T.Text [AsmStmt]                 -- ^ Parser result
             -> EDASAsmOutput                           -- ^ Assembler result
edasAssemble parseResult =
  case parseResult of
    Left stuff   -> return $ Left stuff
    Right  stmts -> mkAsmEvalCtx
                    >>= (\initialCtx ->
                          let (finalctx, result) = mapAccumL evalAsmStmt (Right initialCtx) stmts
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
      let currentPC  = ctx ^. asmPC
          atEndOfAsm = ctx ^. endOfAsm
          -- Associate the statement's label with the current program counter
          stmtCtx    = case stmt ^. symLabel of
                         Nothing    -> ctx
                         Just label -> insertSymLabel ctx label currentPC
      in  if not atEndOfAsm then
            case stmt ^. asmOp of
              Nothing              -> ( Right stmtCtx
                                      , stmtAddr .~ currentPC $ stmt
                                      )
              Just (Insn _insn)    -> (Right stmtCtx, stmt)
              -- Don't jamb the statement label into the symbol table just yet. EQU and DEFL handle
              -- their labels differently.
              Just (Pseudo pseudo) -> evalPseudo ctx stmt pseudo
          else
            -- Ignore everything after the 'END' pseudo-operation
            (ictx, stmt)

-- | Evaluate and generate code for pseudo-operations
evalPseudo :: AsmEvalCtx
           -> AsmStmt
           -> EDASPseudo
           -> (IntermediateCtx, AsmStmt)
evalPseudo ctx stmt pseudo =
    let stmtCtx   = case stmt ^. symLabel of
                        Nothing    -> ctx
                        Just label -> insertSymLabel ctx label (ctx ^. asmPC)
    in  case pseudo of
          -- Equate uses the original assembler context, statement label is stored in the context's equateTab
          Equate expr     -> (evalEquate (stmt ^. symLabel) expr ctx, stmt)
          Origin org      -> (liftM (\o -> asmPC .~ o $ stmtCtx) (evalAsmExpr stmtCtx org), stmt)
          DefB args       -> let cvtDBValue (DBStr str)   = Right $ map charIntegral $ T.unpack str
                                 cvtDBValue (DBExpr expr) = liftM (: []) (evalAsmExprWord8 stmtCtx expr)
                             in  evalDefXXArgs args cvtDBValue stmtCtx stmt
          DefC rept fill  -> evalDefC rept fill stmtCtx stmt
          DefS rept       -> evalDefS rept stmtCtx stmt
          DefW args       -> let cvtDWValue (DWChars c1 c2) = Right $ [(charIntegral c1), (charIntegral c2)]
                                 cvtDWValue (DWExpr expr)   = liftM word2ByteList (evalAsmExpr stmtCtx expr)
                             in  evalDefXXArgs args cvtDWValue stmtCtx stmt
          DSym sym        -> emitText sym stmtCtx stmt
          DExp expr       -> case liftM word2ByteList (evalAsmExpr stmtCtx expr) of
                               Left issues -> (Left issues, stmt)
                               Right words -> updateCtxStmtBytes stmtCtx stmt (DVU.fromList words)
          AsmDate         -> emitString (stmtCtx ^. dateTime & formatTime defaultTimeLocale "%D") stmtCtx stmt
          AsmTime         -> emitString (stmtCtx ^. dateTime & formatTime defaultTimeLocale "%T") stmtCtx stmt
          -- DefL uses the original assembler context, statement label is stored in the context's defLabelTab
          DefL expr       -> (evalDefLabel (stmt ^. symLabel) expr ctx, stmt)
          EndAsm loc expr -> evalEndAsm ctx stmt loc expr
          Entry  loc expr -> evalEntry ctx stmt loc expr

-- | Update statement with its byte vector, context with the updated program counter
updateCtxStmtBytes :: AsmEvalCtx
                   -> AsmStmt
                   -> DVU.Vector Z80word
                   -> (IntermediateCtx, AsmStmt)
updateCtxStmtBytes ctx stmt byteVec = ( Right $ asmPC %~ (+ (fromIntegral . DVU.length) byteVec) $ ctx
                                      , stmtAddr .~ (ctx ^. asmPC) $ bytes .~ byteVec $ stmt
                                      )

-- | Evaluate a symbol equate
evalEquate :: Maybe EDASLabel
           -> EDASExpr
           -> AsmEvalCtx
           -> IntermediateCtx
evalEquate Nothing    _    _   = Left "Equate is missing symbol to which to assign a result."
evalEquate (Just sym) expr ctx = if not (existsEquate ctx sym) then
                                   liftM (insertEquate ctx sym) (evalAsmExpr ctx expr)
                                 else
                                   Left (T.append sym ": cannot redefine an already equated symbol")

-- | Evaluate a defined label
evalDefLabel :: Maybe EDASLabel
             -> EDASExpr
             -> AsmEvalCtx
             -> IntermediateCtx
evalDefLabel Nothing    _    _   = Left "DEFL is missing a symbol to which to assign a result."
evalDefLabel (Just sym) expr ctx = liftM (insertDefLabel ctx sym) (evalAsmExpr ctx expr)

-- | Evaluate the "db"/"defb", "dw"/"defw" argument lists and fold the generated bytes into the assembler statement
evalDefXXArgs :: [argType]
              -> (argType -> Either T.Text [Z80word])
              -> AsmEvalCtx
              -> AsmStmt
              -> (IntermediateCtx, AsmStmt)
evalDefXXArgs args cvtFunc ctx stmt =
        let (ls, rs)                 = partitionEithers $ map cvtFunc args
            theBytes                 = DVU.concat $ map DVU.fromList rs
        in  if (not . null) ls then
              -- Only report the first error
              (Left $ head ls, stmt)
            else
              -- No error: move the program counter forward, save the bytes generated
              updateCtxStmtBytes ctx stmt theBytes

-- | Define constant fill/block
evalDefC :: EDASExpr
         -> EDASExpr
         -> AsmEvalCtx
         -> AsmStmt
         -> (IntermediateCtx, AsmStmt)
evalDefC rept fill ctx stmt =
  let reptVal = evalAsmExpr ctx rept
      fillVal = evalAsmExprWord8 ctx fill
  in  case reptVal of
        Left reptErr -> (Left reptErr, stmt)
        Right rval   -> case fillVal of
                          Left fillErr -> (Left fillErr, stmt)
                          Right fval   -> updateCtxStmtBytes ctx stmt (DVU.replicate (fromIntegral rval) fval)

-- | Define aribtrary space
evalDefS :: EDASExpr
         -> AsmEvalCtx
         -> AsmStmt
         -> (IntermediateCtx, AsmStmt)
evalDefS rept ctx stmt = case evalAsmExpr ctx rept of
                           Left reptErr -> (Left reptErr, stmt)
                           Right rval   -> updateCtxStmtBytes ctx stmt (DVU.replicate (fromIntegral rval) (0 :: Z80word))

-- | Convert a string to bytes, update statement and intermediate assembler context.
emitString :: String
           -> AsmEvalCtx
           -> AsmStmt
           -> (IntermediateCtx, AsmStmt)
emitString str ctx stmt = updateCtxStmtBytes ctx stmt (DVU.fromList (stringToWords str))

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

-- | Set end of assembly flag, update start address if not already set.
evalEndAsm :: AsmEvalCtx
           -> AsmStmt
           -> SourcePos
           -> Maybe EDASExpr
           -> (IntermediateCtx, AsmStmt)
evalEndAsm ctx stmt loc expr =
  let stmtCtx = endOfAsm .~ True $ ctx
  in  case expr of
        Just toEval -> case evalAsmExpr stmtCtx toEval of
                         Left issues -> (Left issues, stmt)
                         Right val   ->
                           let stmtCtx' = if isNothing (stmtCtx ^. startAddr) then
                                            stmtCtx
                                          else
                                            appendWarning stmtCtx loc
                                                          (T.append "Start address already set to "
                                                                    ((asHex . fromJust) $ stmtCtx ^. startAddr))
                           in  (Right (startAddr .~ (Just val) $ stmtCtx')
                               , stmt
                               )
        Nothing     -> (Right stmtCtx, stmt)

-- | Update the start address specified by an \'ENTRY\' pseudo-operation
evalEntry :: AsmEvalCtx
          -> AsmStmt
          -> SourcePos
          -> EDASExpr
          -> (IntermediateCtx, AsmStmt)
evalEntry ctx stmt loc expr =
  case evalAsmExpr ctx expr of
    Left issues -> (Left issues, stmt)
    Right val   -> let stmtCtx = startAddr .~ (Just val) $ if isNothing (ctx ^. startAddr) then
                                                             ctx
                                                           else
                                                             appendWarning ctx loc
                                                                           (T.append "Start address already set to "
                                                                                     (ctx ^. startAddr & (asHex . fromJust)))
                   in  (Right stmtCtx , stmt)
    
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
evalAsmExpr ctx  CurrentPC           = Right (ctx ^. asmPC)
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

-- | Append a new warning onto the existing assembler warnings
appendWarning :: AsmEvalCtx
              -> SourcePos
              -> T.Text
              -> AsmEvalCtx
appendWarning ctx loc msg = warnings %~ (++ [T.append (mkSourcePosT loc) msg]) $ ctx
