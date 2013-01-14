{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Z80 mnmonic/instruction set parser for the Misosys EDAS-compatible assembler
module Z80.MisosysEDAS.MnemonicParser
  ( asmMnemonic
  ) where

{-
#ifdef mingw32_HOST_OS
import Control.Lens hiding (value, walk, op)
#else
import Control.Lens hiding (value, walk)
#endif
-}

import Text.Parsec
import Text.Parsec.Text()                 -- ghci 7.6.1 needs these imported instances, e.g., (Stream T.Text Identity Char)
import qualified Data.Text as T
import qualified Data.Map as Map

import Machine.EmulatedSystem
import Z80.InstructionSet
import Z80.MisosysEDAS.Types
import Z80.MisosysEDAS.ParserUtils
import Z80.MisosysEDAS.ExprParser
import Z80.MisosysEDAS.Assembler

-- | Parse a Z80 assembler opcode
asmMnemonic :: EDASParser AsmOp
asmMnemonic = 
  do { _ <- stringIC "ld"
     ; _ <- whiteSpace
     -- Treat accumulator loads a little differently from other 8-bit register loads, since A can be loaded
     -- indirectly from memory and other registers. Could have 'accumLoad' try and fail, but seems like that
     -- would be a slight performance hit that can easily be avoided with shared code.
     ; accumLoad
       <|> reg8Load
     }
  <|> do { _ <- stringIC "call"
         ; _ <- whiteSpace
         ; calldest <- asmExpr
         -- Evaluation context is curried into the last argument of absAddrEval
         ; evalInsn $ absAddrEval CALL calldest
         }
  <|> do { _ <- charIC 'r'
         ; do { _ <- stringIC "et"
              ; noEval RET
              }
           <|> do { _ <- stringIC "st"
                  ; optional whiteSpace
                  ; rstVecConst <- asmExpr
                  ; evalInsn (\ctx ->  either (\errs -> Left errs)
                                              (\rstVec ->
                                                 if rstVec `elem` [ 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38 ] then
                                                   Right $ RST rstVec
                                                 else
                                                   Left "Invalid RST restart vector value"
                                              )
                                              (evalAsmExprWord8 rstVecConst ctx)
                             )
                  } <?> "RET or RST instruction"
         }
  <|> do { _ <- stringIC "nop"
         ; noEval NOP
         }
  where
    -- Shorthand...
    evalInsn = return . InsnEval

    -- Simple case when an instruction doesn't have an expression to evaluate
    noEval = return . Insn

    -- Simple case for 'AbsAddr'. Note that the 'AbsAddr' part of the data constructor is generally curried as the last
    -- argument to the 'Z80instruction'
    absAddrEval addrIns expr ctx =
      either (\errs -> Left errs)
             (\val -> Right ((addrIns . AbsAddr) val))
             (evalAsmExpr expr ctx)

    -- Simple case for 8-bit constants. This assumes that the 8-bit value is curried into the last argument of the 'insn'
    -- data constructor
    const8Eval insn expr ctx =
      either (\errs -> Left errs)
             (\val  -> Right (insn val))
             (evalAsmExprWord8 expr ctx)

    -- Accumulator loads
    accumLoad = try ( -- Accumulator is destination
                      do { _ <- charIC 'a'
                         ; notFollowedBy alphaNum
                         ; optional whiteSpace
                         ; _ <- char ','
                         ; optional whiteSpace
                         ; reg8LoadSources A
                         }
                    )
    -- 8-bit bit register/register loads
    reg8Load = do { dstReg <- parse8BitReg
                  ; optional whiteSpace
                  ; _ <- char ','
                  ; optional whiteSpace
                  ; reg8LoadSources dstReg
                  }

    -- Shared function between accumulator loads and 8-bit register loads
    reg8LoadSources dstReg = do { srcReg <- parse8BitReg
                                ; noEval (LD (Reg8Reg8 dstReg srcReg))
                                }
                             <|> do { expr <- asmExpr
                                    -- Context is curried into the last argument.
                                    ; evalInsn $ const8Eval (LD . (Reg8Imm dstReg)) expr
                                    }

    -- Parse an 8-bit register name. Valid register names come from the 'Z80.reg8Names' mapping or are an indirect
    -- register operand
    parse8BitReg = try ( do { reg8 <- choice (map (stringIC . T.unpack) (Map.keys reg8NameMap))
                            ; notFollowedBy alphaNum
                            ; return $ reg8NameToReg (T.pack reg8)
                            }
                         -- Handle the indirect register operands, (HL), (IX+d) and (IY+d)
                         <|> indirectReg8
                       )

    -- Indirect 8-bit register load/store
    indirectReg8 = between (char '(') (char ')')
                           ( do { optional whiteSpace
                                ; _ <- stringIC "hl"
                                ; optional whiteSpace
                                ; return HLindirect
                                }
                             <|> do { optional whiteSpace
                                    ; _ <- stringIC "ix"
                                    ; disp <- parseDisplacement
                                    ; optional whiteSpace
                                    ; return $ IXindirect disp
                                    }
                             <|> do { optional whiteSpace
                                    ; _ <- stringIC "iy"
                                    ; optional whiteSpace
                                    ; disp <- parseDisplacement
                                    ; return $ IYindirect disp
                                    }
                            )
    -- Parse an indexed displacement
    parseDisplacement = do { srcpos <- getPosition
                           ; c <- constExpr True srcpos
                           ; case c of
                               Const _srcloc disp -> if disp >= -128 && disp <= 127 then
                                                       return disp
                                                     else
                                                       fail "index displacement out of bounds"
                               _otherwise  -> fail "index register displacement"
                            }
