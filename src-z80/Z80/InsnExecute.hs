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
import           Z80.Processor         (Z80addr, Z80state, Z80byte, z80pc)
import           Z80.System            ()

z80instructionExecute :: InstructionExecute Z80state Z80instruction Z80addr Z80byte
z80instructionExecute insn {-sys-} =
  case decodedIns of
    Z80undef _       -> nextInsnPC {-sys-}
    LD opnd          -> nextInsnPC . insLoad         opnd {-sys-}
    INC opnd         -> nextInsnPC . insInc          opnd False {-sys-}
    DEC opnd         -> nextInsnPC . insDec          opnd True  {-sys-}
    AND opnd         -> nextInsnPC . aluAccumOp      opnd (.&.)      False {-sys-}
    XOR opnd         -> nextInsnPC . aluAccumOp      opnd xor        False {-sys-}
    OR opnd          -> nextInsnPC . aluAccumOp      opnd (.|.)      False {-sys-}
    ADD opnd         -> nextInsnPC . aluAccumOp      opnd (+)        True  {-sys-}
    SUB opnd         -> nextInsnPC . aluAccumOp      opnd (-)        True  {-sys-}
    -- Back up the PC by one: HALT is basically an infinite loop until an interrupt occurs
    HALT             -> updatePC (insnPC - 1) {-sys-}
    NOP{}            -> nextInsnPC {-sys-}
    _                -> nextInsnPC {-sys-}
  where
    decodedIns = insn ^. decodedInsn
    insnPC = insn ^. decodedInsnPC
    nextInsnPC = updatePC insnPC
    updatePC addr sys' = sys' & processor . cpu . z80pc .~ addr
