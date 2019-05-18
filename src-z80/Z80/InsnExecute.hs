module Z80.InsnExecute
  ( z80instructionExecute

  -- * Test harness exports
  , reg16get
  , reg16set
  , make16bit
  )
where

import Data.Bits
import Lens.Micro.Platform

import Machine
import Z80.Execute.ALUops
import Z80.Execute.IncDec
import Z80.Execute.LoadStore
import Z80.Execute.Utils
import Z80.InstructionSet
import Z80.Processor

-- import Debug.Trace
-- import Text.Printf

z80instructionExecute :: InstructionExecute Z80state Z80instruction Z80addr Z80word
z80instructionExecute insn sys =
  case decodedIns of
    Z80undef _ -> nextInsnPC sys
    LD opnd    -> nextInsnPC $ insLoad opnd sys
    INC opnd   -> nextInsnPC $ insIncDec opnd (+ 1)      False sys
    DEC opnd   -> nextInsnPC $ insIncDec opnd (+ 0xff)   True  sys
    INC16 opnd -> nextInsnPC $ insIncDec16 opnd (+ 1)    False sys
    DEC16 opnd -> nextInsnPC $ insIncDec16 opnd (+ 0xff) True  sys
    SUB opnd   -> nextInsnPC $ aluAccumOp opnd (-)       True  sys
    AND opnd   -> nextInsnPC $ aluAccumOp opnd (.&.)     False sys
    XOR opnd   -> nextInsnPC $ aluAccumOp opnd xor       False sys
    OR opnd    -> nextInsnPC $ aluAccumOp opnd (.|.)     False sys
    -- Back up the PC by one: HALT is basically an infinite loop until an interrupt occurs
    HALT       -> updatePC (insnPC - 1) sys
    NOP        -> nextInsnPC sys
    _          -> nextInsnPC sys
  where
    decodedIns = insn ^. decodedInsn
    insnPC = insn ^. decodedInsnPC
    nextInsnPC = updatePC insnPC
    updatePC addr sys' = sys' & processor . cpu . z80pc .~ addr
