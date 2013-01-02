module Z80.MisosysEDAS.Assembler 
  ( edasAssemble
  , evalAsmExpr
  , evalAsmExprToWord8
  ) where

import Debug.Trace

import Control.Lens hiding (value)
import Data.Word
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T

import Z80.Processor
import Z80.MisosysEDAS.Types

-- | Shorthand for catching assembler issues and problems
type IntermediateCtx = Either T.Text AsmEvalCtx

-- | The assembler pass: Evaluate the pseudo-operations and expressions within instructions, update the assembler
-- statement with the results and 'Z80instruction's.
edasAssemble :: Either T.Text [AsmStmt]                 -- ^ Parser result
             -> Either T.Text (AsmEvalCtx, [AsmStmt])   -- ^ Assembler result
edasAssemble parseResult =
  case parseResult of
    Left stuff   -> Left stuff
    Right  stmts -> let initialCtx = Right $ AsmEvalCtx { _symbolTab = Map.empty
                                                        , _asmPC     = 0
                                                        }
                        (finalctx, result) = mapAccumL evalAsmStmt initialCtx stmts
                    in  case finalctx of
                          Left stuff   -> Left stuff
                          Right  ctx   -> Right (ctx, result)

-- | Evaluate a single assembler statement
evalAsmStmt :: IntermediateCtx
            -> AsmStmt
            -> (IntermediateCtx, AsmStmt)
evalAsmStmt ictx stmt =
  case ictx of
    Left stuff  -> (Left stuff, stmt)
    Right ctx   -> case stmt ^. asmOp of
                     Nothing              -> (Right ctx, stmt)
                     Just (Insn insn)     -> (Right ctx, stmt)
                     Just (Pseudo pseudo) -> evalPseudo ctx stmt pseudo

evalPseudo :: AsmEvalCtx
           -> AsmStmt
           -> EDASPseudo
           -> (IntermediateCtx, AsmStmt)
evalPseudo ctx stmt pseudo =
    case pseudo of
      Equate expr -> (evalEquate (stmt ^. symLabel) expr ctx, stmt)
      Origin org  -> undefined

-- | Evaluate a symbol equate
evalEquate :: Maybe EDASLabel
           -> EDASExpr
           -> AsmEvalCtx
           -> IntermediateCtx
evalEquate Nothing _ _ = Left "Equate is missing symbol to which to assign result."
evalEquate (Just sym) expr ctx =
  case evalAsmExpr ctx expr of
    Left problem -> Left problem
    Right value  -> Right $ insertAsmSymbol ctx sym value

-- | Evaluate an assembler expression to produce a 'Word16' result, within the current assembler evaluation context
evalAsmExpr :: AsmEvalCtx
            -> EDASExpr
            -> Either T.Text Word16
evalAsmExpr _ctx (Const cst) = Right (fromIntegral cst)
evalAsmExpr  ctx (Var   v)   = case findAsmSymbol ctx v of
                                 Nothing -> Left (T.append "Unknown symbol name: " v)
                                 Just x  -> Right x
evalAsmExpr ctx  (Add l r)   = evalBinOp ctx (+) l r
evalAsmExpr ctx  (Sub l r)   = evalBinOp ctx (-) l r
evalAsmExpr ctx  (Mul l r)   = evalBinOp ctx (*) l r
evalAsmExpr _ctx _           = Left "unimplemented"

-- | Workhorse for evaluating binary operations. Note that EDAS never had a concept of operator precedence, so this is
-- very straightforward.
evalBinOp :: AsmEvalCtx
          -> (Word16 -> Word16 -> Word16)
          -> EDASExpr
          -> EDASExpr
          -> Either T.Text Word16
evalBinOp ctx op l r
  | Left lproblem <- lval = Left lproblem
  | Left rproblem <- rval = Left rproblem
  | otherwise             = let Right lval' = lval
                                Right rval' = rval
                            in  Right $ lval' `op` rval'
  where
    lval = evalAsmExpr ctx l
    rval = evalAsmExpr ctx r

-- | Evaluate an assembler expression to produce a 'Word8' result, within the current assembler evaluation context
evalAsmExprToWord8 :: AsmEvalCtx
                   -> EDASExpr
                   -> Either T.Text Z80word
evalAsmExprToWord8 = undefined
