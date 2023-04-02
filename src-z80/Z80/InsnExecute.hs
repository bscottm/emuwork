module Z80.InsnExecute
  ( z80instructionExecute

  -- * Test harness exports
  , reg16get
  , reg16set
  , make16bit
  )
where

import           Data.Bits             (Bits (xor, (.&.), (.|.)))

import           Lens.Micro.Platform   ((&), (.~), (^.))

import           Machine               (InstructionExecute, cpu, decodedInsn, decodedInsnPC, processor)

import Z80.Execute.ALUops
import Z80.Execute.IncDec
import Z80.Execute.LoadStore
import           Z80.Execute.Utils     (make16bit, reg16get, reg16set)
import           Z80.InstructionSet
import           Z80.Processor         (Z80addr, Z80state, Z80word, z80pc)
import           Z80.System            ()

z80instructionExecute :: InstructionExecute Z80state Z80instruction Z80addr Z80word
z80instructionExecute insn {-sys-} =
  case decodedIns of
    Z80undef _       -> nextInsnPC {-sys-}
    LDAspecial opnd  -> nextInsnPC . insLoad         opnd {-sys-}
    LDr8r8 opnd      -> nextInsnPC . insLoadReg8Reg8 opnd {-sys-}
    LDr8imm opnd     -> nextInsnPC . insLoadReg8Imm  opnd {-sys-}
    LDAmem opnd      -> nextInsnPC . insLoadAMemXfer opnd {-sys-}
    LDr16mem opnd    -> nextInsnPC . insLoadReg16Mem opnd {-sys-}
    LDr16imm opnd    -> nextInsnPC . insLoadReg16Imm opnd {-sys-}
    INC opnd         -> nextInsnPC . insIncDec       opnd (+ 1)      False {-sys-}
    DEC opnd         -> nextInsnPC . insIncDec       opnd (+ 0xff)   True  {-sys-}
    INC16 opnd       -> nextInsnPC . insIncDec16     opnd (+ 1)      False {-sys-}
    DEC16 opnd       -> nextInsnPC . insIncDec16     opnd (+ 0xff)   True  {-sys-}
    AND opnd         -> nextInsnPC . aluAccumOp      opnd (.&.)      False {-sys-}
    XOR opnd         -> nextInsnPC . aluAccumOp      opnd xor        False {-sys-}
    OR opnd          -> nextInsnPC . aluAccumOp      opnd (.|.)      False {-sys-}
    ADD8 opnd        -> nextInsnPC . aluAccumMathOp  opnd (+)        True  {-sys-}
    SUB8 opnd        -> nextInsnPC . aluAccumMathOp  opnd (-)        True  {-sys-}
    -- Back up the PC by one: HALT is basically an infinite loop until an interrupt occurs
    HALT             -> updatePC (insnPC - 1) {-sys-}
    NOP              -> nextInsnPC {-sys-}
    _                -> nextInsnPC {-sys-}
  where
    decodedIns = insn ^. decodedInsn
    insnPC = insn ^. decodedInsnPC
    nextInsnPC = updatePC insnPC
    updatePC addr sys' = sys' & processor . cpu . z80pc .~ addr
