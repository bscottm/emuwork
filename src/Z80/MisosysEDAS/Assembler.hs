{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Z80.MisosysEDAS.Assembler 
  ( -- * Types and Data Constructors
    EDASAsmOutput

    -- * Functions
  , edasAssemble
  , evalAsmExpr
  , evalAsmExprWord8
  ) where

-- import Debug.Trace

import Prelude hiding (words)
import Control.Monad
import Control.Lens hiding (op)
import Data.Either
import Data.Maybe
import Data.Word
import Data.Int
import Data.List hiding (words)
import Data.Bits
import Data.Time
-- import System.Locale
-- import Data.Time.Format
import Text.Parsec.Pos
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as DVU

import Machine.Utils
import Z80.Processor
import Z80.MisosysEDAS.Types

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
                              noEndPseudoOp      = "No 'END' pseudo-operation encountered at end of source code."
                              noStartAddr        = "No start address for execution specified."
                              mkResult ctx       = if not (ctx ^. endOfAsm) then
                                                     Left noEndPseudoOp
                                                   else if isNothing (ctx ^. startAddr) then
                                                     Left noStartAddr
                                                   else
                                                     Right ( ctx, result )
                          in  return $ either Left
                                              mkResult
                                              finalctx
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
                         Nothing    -> Right ctx
                         Just label -> if not (existsSymLabel label ctx) then
                                         Right $ insertSymLabel label currentPC ctx
                                       else
                                         Left $ T.concat [ "Duplicate assembler/statement label: '"
                                                         , label
                                                         , "'"
                                                         ]
      in  if not atEndOfAsm then
            case stmt of
              (AsmStmt _ _ NoAsmOp          _ _ _) -> ( stmtCtx
                                                   , stmtAddr .~ currentPC $ stmt
                                                   )
              (AsmStmt _ _ (InsnEval _insn) _ _ _) -> ( stmtCtx, stmt )
              (AsmStmt _ _ (Insn     _insn) _ _ _) -> ( stmtCtx, stmt )
              -- Don't jamb the statement label into the symbol table via stmtCtx just yet. EQU and DEFL handle
              -- their labels in a special way (EQU ensures that symbols are only equated once, DEFL allows symbol
              -- redefinition.)
              (AsmStmt _ _ (Pseudo  pseudo) _ _ _) -> evalPseudo ctx stmt pseudo
              (AsmStmt _ _ (AsmSeq   _aseq) _ _ _) -> ( Left "AsmSeq referenced", stmt )
              (CondPass _ _ _ _ _ _)               -> ( stmtCtx, stmt )
              (CondCmp _ _ _ _ _ _ _ _ _)          -> ( stmtCtx, stmt )
              (CondCmpStr _ _ _ _ _ _ _ _ _)       -> ( stmtCtx, stmt )
              (CondAsmEval _ _ _ _ _ _ _ _ _ _ _)  -> evalCondAsmEval stmtCtx stmt
          else
            -- Ignore everything after the 'END' pseudo-operation
            (ictx, stmt)

-- | Evaluate and generate code for pseudo-operations
evalPseudo :: AsmEvalCtx
           -> AsmStmt
           -> EDASPseudo
           -> (IntermediateCtx, AsmStmt)
evalPseudo ctx stmt pseudo =
    let theSymLabel          = stmt ^. symLabel
        stmtCtx              = case theSymLabel of
                                 Nothing    -> defaultCtx
                                 Just label -> if not (existsSymLabel label ctx) then
                                                 Right $ insertSymLabel label (ctx ^. asmPC) ctx
                                               else
                                                  Left $ T.concat [ "Duplicate assembler/statement label "
                                                                  , (T.cons '"' (T.snoc label '"'))
                                                                  ]
        defaultCtx           = Right ctx
        asmDateTime ctx' fmt = ctx' ^. dateTime & formatTime defaultTimeLocale fmt
    in  case pseudo of
          -- Equate uses the original assembler context, statement label is stored in the context's equateTab
          Equate expr     -> (evalEquate (stmt ^. symLabel) expr defaultCtx, stmt)
          Origin orgExpr  -> -- Origin cannot have a statement label
                             if isJust theSymLabel then
                               ( Left (T.append (mkSourcePosT (stmt ^. srcPos))
                                                (T.append ": Illegal ORG label " 
                                                          (T.cons '"' (T.snoc (fromJust theSymLabel) '"'))))
                               , stmt
                               )
                             else ( -- Thread 'Either' through multple monadic hoops:
                                   liftM2 (\origin ctx' -> asmPC .~ origin $ ctx') (evalAsmExpr orgExpr stmtCtx) stmtCtx
                                  , stmt 
                                  )
          DefB args       -> let cvtDBValue (DBStr str)   = Right $ map charIntegral $ T.unpack str
                                 cvtDBValue (DBExpr expr) = liftM (: []) (evalAsmExprWord8 expr stmtCtx)
                             in  evalDefXXArgs args cvtDBValue stmt stmtCtx
          DefC rept fill  -> evalDefC rept fill stmt stmtCtx
          DefS rept       -> evalDefS rept stmt stmtCtx
          DefW args       -> let cvtDWValue (DWChars c1 c2) = Right $ [charIntegral c1, charIntegral c2]
                                 cvtDWValue (DWExpr expr)   = liftM word2ByteList (evalAsmExpr expr stmtCtx)
                             in  evalDefXXArgs args cvtDWValue stmt stmtCtx
          DSym sym        -> emitText sym stmt stmtCtx
          DExp expr       -> case liftM word2ByteList (evalAsmExpr expr stmtCtx) of
                               Left issues -> (Left issues, stmt)
                               Right words -> updateCtxStmtBytes stmt (DVU.fromList words) stmtCtx 
          AsmDate         -> either (propagateErrs stmt)
                                    (\ctx' -> emitString (asmDateTime ctx' "%D") stmt stmtCtx) stmtCtx
          AsmTime         -> either (propagateErrs stmt)
                                    (\ctx' -> emitString (asmDateTime ctx' "%T") stmt stmtCtx) stmtCtx
          -- DefL uses the original assembler context, statement label is stored in the context's defLabelTab
          DefL expr       -> (evalDefLabel (stmt ^. symLabel) expr defaultCtx, stmt)
          EndAsm loc expr -> evalEndAsm stmt loc expr stmtCtx
          Entry  loc expr -> evalStartAddr stmt loc expr stmtCtx
          -- Currently, we don't do anything with the load origin's program counter beyond noting that it
          -- was set to some value.
          LoadOrg expr    -> ( liftM2 (\o ctx' -> lorgPC .~ o $ ctx') (evalAsmExpr expr stmtCtx) stmtCtx
                             , stmt 
                             )

-- | Update statement with its byte vector, context with the updated program counter
updateCtxStmtBytes :: AsmStmt
                   -> DVU.Vector Z80word
                   -> IntermediateCtx
                   -> (IntermediateCtx, AsmStmt)
updateCtxStmtBytes stmt byteVec ctx =
  let updateStmt ctx'     = ( Right $ asmPC %~ (+ (fromIntegral . DVU.length) byteVec) $ ctx'
                            , stmtAddr .~ (ctx' ^. asmPC) $ bytes .~ byteVec $ stmt
                            )
  in  either (propagateErrs stmt) updateStmt ctx

-- | Evaluate a symbol equate
evalEquate :: Maybe EDASLabel
           -> EDASExpr
           -> IntermediateCtx
           -> IntermediateCtx
evalEquate Nothing    _    _   = Left "Equate is missing symbol to which to assign a result."
evalEquate (Just sym) expr ctx = 
  ctx >>= (\ctx' -> if not (existsEquate sym ctx') then
                      liftM (\val -> insertEquate sym val ctx') (evalAsmExpr expr ctx)
                    else
                      Left (T.append sym ": cannot redefine an already equated symbol")
          )

-- | Evaluate a defined label
evalDefLabel :: Maybe EDASLabel
             -> EDASExpr
             -> IntermediateCtx
             -> IntermediateCtx
evalDefLabel Nothing    _    _   = Left "DEFL is missing a symbol to which to assign a result."
evalDefLabel (Just sym) expr ctx = liftM2 (\symval ctx' -> insertDefLabel sym symval ctx') (evalAsmExpr expr ctx) ctx

-- | Evaluate the "db"/"defb", "dw"/"defw" argument lists and fold the generated bytes into the assembler statement
evalDefXXArgs :: [argType]
              -> (argType -> Either T.Text [Z80word])
              -> AsmStmt
              -> IntermediateCtx
              -> (IntermediateCtx, AsmStmt)
evalDefXXArgs args cvtFunc stmt ctx =
  let (ls, rs)                 = partitionEithers $ map cvtFunc args
      theBytes                 = DVU.concat $ map DVU.fromList rs
  in  if (not . null) ls then
        (Left $ T.intercalate "\n" ls, stmt)
      else
        -- No error: move the program counter forward, save the bytes generated
        updateCtxStmtBytes stmt theBytes ctx

-- | Define constant fill/block
evalDefC :: EDASExpr
         -> EDASExpr
         -> AsmStmt
         -> IntermediateCtx
         -> (IntermediateCtx, AsmStmt)
evalDefC rept fill stmt ctx =
  let reptVal = evalAsmExpr rept ctx
      fillVal = evalAsmExprWord8 fill ctx
  in  case reptVal of
        Left reptErr -> (Left reptErr, stmt)
        Right rval   -> case fillVal of
                          Left fillErr -> (Left fillErr, stmt)
                          Right fval   -> updateCtxStmtBytes stmt (DVU.replicate (fromIntegral rval) fval) ctx

-- | Define aribtrary space
evalDefS :: EDASExpr
         -> AsmStmt
         -> IntermediateCtx
         -> (IntermediateCtx, AsmStmt)
evalDefS rept stmt ctx = case evalAsmExpr rept ctx of
                           Left reptErr -> (Left reptErr, stmt)
                           Right rval   -> updateCtxStmtBytes stmt (DVU.replicate (fromIntegral rval) (0 :: Z80word)) ctx

-- | Convert a string to bytes, update statement and intermediate assembler context.
emitString :: String
           -> AsmStmt
           -> IntermediateCtx
           -> (IntermediateCtx, AsmStmt)
emitString str stmt ctx = updateCtxStmtBytes stmt (DVU.fromList (stringToWords str)) ctx

-- | Emit 'Data.Text' string as a sequence of bytes
emitText :: T.Text
         -> AsmStmt
         -> IntermediateCtx
         -> (IntermediateCtx, AsmStmt)
emitText = emitString . T.unpack

-- | Convert word to list of bytes in little endian order 
word2ByteList :: Word16
              -> [Z80word]
word2ByteList w = [(fromIntegral (w .&. 0xff)), (fromIntegral (w `shiftR` 8))]

-- | Set end of assembly flag, update start address if not already set.
evalEndAsm :: AsmStmt
           -> SourcePos
           -> Maybe EDASExpr
           -> IntermediateCtx
           -> (IntermediateCtx, AsmStmt)
evalEndAsm stmt loc expr ctx =
  let stmtCtx              = liftM (\ctx' -> endOfAsm .~ True $ ctx') ctx
      hasStartAddress ctx' = if isNothing (ctx' ^. startAddr) then
                               appendWarning ctx' (Just loc) "'END' encountered, no start address set."
                             else
                               ctx'
  in  case expr of
        Just toEval -> evalStartAddr stmt loc toEval stmtCtx
        Nothing     -> ( liftM hasStartAddress stmtCtx
                       , stmt
                       )

-- | Update the start address. This is invoked directly by an \'ENTRY\' pseudo-operation
evalStartAddr :: AsmStmt
              -> SourcePos
              -> EDASExpr
              -> IntermediateCtx
              -> (IntermediateCtx, AsmStmt)
evalStartAddr stmt loc expr ctx =
  let evalCtx             = evalAsmExpr expr ctx
      startAddrT ctx'     = ctx' ^. startAddr & (asHex . fromJust)
      mkReturnCtx ctx'    = if isNothing (ctx' ^. startAddr) then
                              ctx'
                            else
                              appendWarning ctx' (Just loc) (T.append "Start address already set to " (startAddrT ctx'))
      updateStartAddr val = ( liftM (\ctx' -> startAddr .~ (Just val) $ (mkReturnCtx ctx')) ctx
                            , stmt
                            )
  in  either (propagateErrs stmt) updateStartAddr evalCtx

-- | Evaluate a simple conditional assembly expression: IF <expr>, where <expr> == (0/!0)
evalCondAsmEval :: IntermediateCtx
                -> AsmStmt
                -> (IntermediateCtx, AsmStmt)
evalCondAsmEval ictx
                stmt@(CondAsmEval _ _ _ _ _ _ _ _ _ _ _) =
  either (propagateErrs stmt)
         (\val -> let boolVal       = val /= 0
                  in annotateEndIfLabel (evalCondStatements ictx (condResult .~ boolVal $ stmt) boolVal)
         )
         (evalAsmExpr (stmt ^. evalExp) ictx)

annotateEndIfLabel :: Monad m
                   => (m AsmEvalCtx, AsmStmt)
                   -> (m AsmEvalCtx, AsmStmt)
annotateEndIfLabel evalResult@( newCtx, newStmt ) =
  let theEndifLabel = newStmt ^. endifLabel
  in  if isJust theEndifLabel then
        ( liftM (\ctx -> insertSymLabel (fromJust theEndifLabel) (ctx ^. asmPC) ctx) newCtx
        , newStmt
        )
      else
        evalResult

annotateElseLabel :: Monad m
                  => (m AsmEvalCtx, AsmStmt)
                  -> (m AsmEvalCtx, AsmStmt)
annotateElseLabel evalResult@( newCtx, newStmt ) =
  let theElseLabel = newStmt ^. elseLabel
  in  if isJust theElseLabel then
        ( liftM (\ctx -> insertSymLabel (fromJust theElseLabel) (ctx ^. asmPC) ctx) newCtx
        , newStmt
        )
      else
        evalResult

-- | Evaluates the true or false sides of a conditional assembly statement.
evalCondStatements :: IntermediateCtx
                   -> AsmStmt
                   -> Bool
                   -> (IntermediateCtx, AsmStmt)

evalCondStatements ictx stmt True =
  let ( trueCtx, stmts' ) = mapAccumL evalAsmStmt ictx (stmt ^. stmtsTrue)
  in  annotateElseLabel ( trueCtx, (stmtsTrue .~ stmts') $ stmt )

evalCondStatements ictx stmt False =
  let ( elseCtx,  elseStmt ) = annotateElseLabel (ictx, stmt)
      ( falseCtx, stmts' ) = mapAccumL evalAsmStmt elseCtx (elseStmt ^. stmtsFalse)
  in  ( falseCtx, (stmtsFalse .~ stmts') $ stmt )

-- | Evaluate an assembler expression to produce a 'Word16' result, within the current assembler evaluation context
evalAsmExpr :: EDASExpr
            -> IntermediateCtx
            -> Either T.Text Word16
evalAsmExpr (Const16 _srcloc cst _base) _ctx = Right (fromIntegral cst)
evalAsmExpr (Var pos v)          ctx       = 
  ctx >>= (\ctx' -> case findAsmSymbol v ctx' of
                      Nothing -> Left (T.concat [ mkSourcePosT pos
                                                , "Unknown equate or label name: "
                                                , v
                                                ]
                                      )
                      Just x  -> Right x
          )
evalAsmExpr CurrentPC ctx    = liftM (\ctx' -> ctx' ^. asmPC) ctx
evalAsmExpr (AsmChar c) _ctx = Right (charIntegral c)
evalAsmExpr (Add l r) ctx    = evalBinOp ctx (+) l r
evalAsmExpr (Sub l r) ctx    = evalBinOp ctx (-) l r
evalAsmExpr (Mul l r) ctx    = evalBinOp ctx (*) l r
evalAsmExpr (Div l r) ctx    = liftM2 (\l' r' -> l' `div` (r' .&. 0xff)) (evalAsmExpr l ctx) (evalAsmExpr r ctx)
evalAsmExpr (Mod l r) ctx    = liftM2 (\l' r' -> l' `mod` (r' .&. 0xff)) (evalAsmExpr l ctx) (evalAsmExpr r ctx)
evalAsmExpr (Shift v x) ctx  = liftM2 (\v' x' -> let x'' = (fromIntegral x') :: Int16
                                                         in  v' `shift` (fromIntegral x''))
                                              (evalAsmExpr v ctx)
                                              (evalAsmExpr x ctx)
evalAsmExpr (LogAnd l r) ctx = evalBinOp ctx (.&.) l r
evalAsmExpr (LogOr  l r) ctx = evalBinOp ctx (.|.) l r
evalAsmExpr (LogXor l r) ctx = evalBinOp ctx xor l r
evalAsmExpr (LogNE  l r) ctx = evalCompare ctx (/=) l r
evalAsmExpr (LogEQ  l r) ctx = evalCompare ctx (==) l r
evalAsmExpr (LogGE  l r) ctx = evalCompare ctx (>=) l r
evalAsmExpr (LogGT  l r) ctx = evalCompare ctx (>)  l r
evalAsmExpr (LogLE  l r) ctx = evalCompare ctx (<=) l r
evalAsmExpr (LogLT  l r) ctx = evalCompare ctx (<)  l r
evalAsmExpr (ShiftL v x) ctx = liftM2 (\v' x' -> v' `shiftL` (fromIntegral x')) (evalAsmExpr v ctx) (evalAsmExpr x ctx)
evalAsmExpr (ShiftR v x) ctx = liftM2 (\v' x' -> v' `shiftR` (fromIntegral x')) (evalAsmExpr v ctx) (evalAsmExpr x ctx)
evalAsmExpr (OnesCpl x)  ctx = evalUnaryOp ctx complement x
evalAsmExpr (HighByte x) ctx = evalUnaryOp ctx (\y -> (y `shiftR` 8) .&. 0xff) x
evalAsmExpr (LowByte x)  ctx = evalUnaryOp ctx (\y -> y .&. 0xff) x

-- | Evaluate a unary operator expression
evalUnaryOp :: IntermediateCtx
            -> (Word16 -> Word16)
            -> EDASExpr
            -> Either T.Text Word16
evalUnaryOp ctx op x = liftM op (evalAsmExpr x ctx)

-- | Evaluate a binary operator expression
evalBinOp :: IntermediateCtx
          -> (Word16 -> Word16 -> Word16)
          -> EDASExpr
          -> EDASExpr
          -> Either T.Text Word16
evalBinOp ctx op l r = liftM2 op (evalAsmExpr l ctx) (evalAsmExpr r ctx)

-- | Evaluate a comparison expression
evalCompare :: IntermediateCtx
            -> (Word16 -> Word16 -> Bool)
            -> EDASExpr
            -> EDASExpr
            -> Either T.Text Word16
evalCompare ctx op l r = liftM2 compareResult (evalAsmExpr l ctx) (evalAsmExpr r ctx)
  where
    compareResult l' r' = if l' `op` r' then
                          0 :: Word16
                        else
                          0xffff :: Word16

-- | Evaluate an assembler expression to produce a 'Word8' result, within the current assembler evaluation context
evalAsmExprWord8 :: EDASExpr
                 -> IntermediateCtx
                 -> Either T.Text Z80word
evalAsmExprWord8 expr ctx = either (\errs -> Left errs) rangeCheck (evalAsmExpr expr ctx)
  where
    rangeCheck x = if (x .&. 0xff00 == 0xff00) || (x <= 0xff) then
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
mkSourcePosT srcpos = let srcLine = sourceLine srcpos
                          srcCol  = sourceColumn srcpos
                          srcFile = sourceName srcpos
                      in  T.append ( T.append "EDAS " (T.cons '"' (T.snoc (T.pack srcFile) '"')))
                                   ( T.concat [ " line "
                                              , (T.pack . show) srcLine
                                              , ", col "
                                              , (T.pack . show) srcCol
                                              , ": "
                                              ]
                                   )

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
              -> Maybe SourcePos
              -> T.Text
              -> AsmEvalCtx
appendWarning ctx loc msg = warnings %~ (++ [T.append (maybe T.empty mkSourcePosT loc) msg]) $ ctx

-- | Helper function for 'Data.Maybe.either'
propagateErrs :: AsmStmt
              -> T.Text
              -> (IntermediateCtx, AsmStmt)
propagateErrs stmt errs  = ( Left errs
                           , stmt
                           )
