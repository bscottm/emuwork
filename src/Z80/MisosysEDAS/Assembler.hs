module Z80.MisosysEDAS.Assembler 
  ( -- * Types and Data Constructors
    EDASAsmOutput

    -- * Functions
  , edasAssemble
  , evalAsmExpr
  , evalAsmExprToWord8
  ) where

import Debug.Trace

import Control.Lens hiding (value, op)
import Control.Monad
import Data.Word
import Data.Int
import Data.List
import Data.Bits
import qualified Data.Map as Map
import qualified Data.Text as T

import Z80.Processor
import Z80.MisosysEDAS.Types

-- | Shorthand for catching intermediate assembler issues and problems
type IntermediateCtx = Either T.Text AsmEvalCtx

-- | Shorthand for the assembler's final result
type EDASAsmOutput = Either T.Text (AsmEvalCtx, [AsmStmt])

-- | The assembler pass: Evaluate the pseudo-operations and expressions within instructions, update the assembler
-- statement with the results and 'Z80instruction's.
edasAssemble :: Either T.Text [AsmStmt]                 -- ^ Parser result
             -> EDASAsmOutput                           -- ^ Assembler result
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
evalEquate (Just sym) expr ctx = liftM (insertAsmSymbol ctx sym) (evalAsmExpr ctx expr)

-- | Evaluate an assembler expression to produce a 'Word16' result, within the current assembler evaluation context
evalAsmExpr :: AsmEvalCtx
            -> EDASExpr
            -> Either T.Text Word16
evalAsmExpr _ctx (Const cst)  = Right (fromIntegral cst)
evalAsmExpr  ctx (Var   v)    = case findAsmSymbol ctx v of
                                  Nothing -> Left (T.append "Unknown symbol name: " v)
                                  Just x  -> Right x
evalAsmExpr ctx  (Add l r)    = evalBinOp ctx (+) l r
evalAsmExpr ctx  (Sub l r)    = evalBinOp ctx (-) l r
evalAsmExpr ctx  (Mul l r)    = evalBinOp ctx (*) l r
evalAsmExpr ctx  (Div l r)    = liftM2 (\l' r' -> l' `div` (r' .&. 0xff)) (evalAsmExpr ctx l) (evalAsmExpr ctx r)
evalAsmExpr ctx  (Mod l r)    = liftM2 (\l' r' -> l' `mod` (r' .&. 0xff)) (evalAsmExpr ctx l) (evalAsmExpr ctx r)
evalAsmExpr ctx  (Shift v x)  = liftM2 (\v' x' -> let x'' = (fromIntegral x') :: Int16
                                                  in  v' `shift` (fromIntegral x''))
                                       (evalAsmExpr ctx v)
                                       (evalAsmExpr ctx x)
evalAsmExpr ctx  (LogAnd l r) = evalBinOp ctx (.&.) l r
evalAsmExpr ctx  (LogOr  l r) = evalBinOp ctx (.|.) l r
evalAsmExpr ctx  (LogXor l r) = evalBinOp ctx xor l r
evalAsmExpr ctx  (LogNE  l r) = evalCompare ctx (/=) l r
evalAsmExpr ctx  (LogEQ  l r) = evalCompare ctx (==) l r
evalAsmExpr ctx  (LogGE  l r) = evalCompare ctx (>=) l r
evalAsmExpr ctx  (LogGT  l r) = evalCompare ctx (>)  l r
evalAsmExpr ctx  (LogLE  l r) = evalCompare ctx (<=) l r
evalAsmExpr ctx  (LogLT  l r) = evalCompare ctx (<)  l r
evalAsmExpr ctx  (ShiftL v x) = liftM2 (\v' x' -> v' `shiftL` (fromIntegral x')) (evalAsmExpr ctx v) (evalAsmExpr ctx x)
evalAsmExpr ctx  (ShiftR v x) = liftM2 (\v' x' -> v' `shiftR` (fromIntegral x')) (evalAsmExpr ctx v) (evalAsmExpr ctx x)
evalAsmExpr ctx  (OnesCpl x)  = evalUnaryOp ctx complement x
evalAsmExpr ctx  (HighByte x) = evalUnaryOp ctx (\y -> (y `shiftR` 8) .&. 0xff) x
evalAsmExpr ctx  (LowByte x)  = evalUnaryOp ctx (\y -> y .&. 0xff) x

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
evalAsmExprToWord8 :: AsmEvalCtx
                   -> EDASExpr
                   -> Either T.Text Z80word
evalAsmExprToWord8 = undefined
