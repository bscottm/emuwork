module Z80.Execute.ALUops
  ( aluAccumOp
  )
where

import Lens.Micro.Platform

import Machine
import Z80.Execute.Utils
import Z80.InstructionSet
import Z80.Processor
import Z80.System


--
aluAccumOp
  :: OperALU
  -> (Z80word -> Z80word -> Z80word)
  -> Bool
  -> Z80system sysTag
  -> Z80system sysTag
aluAccumOp (ALUimm  imm) arithOp nFlag sys               = doALUAccumVal (arithOp imm) nFlag sys
aluAccumOp (ALUreg8 HLindirect) arithOp nFlag sys        = doALUAccumVal (arithOp memVal) nFlag sys'
  where
    (memVal, sys') = sysMRead (reg16get HL sys) sys
aluAccumOp (ALUreg8 (IXindirect disp)) arithOp nFlag sys = doALUAccumVal (arithOp memVal) nFlag sys'
  where
    (memVal, sys') = sysMRead (reg16get IX sys + signExtend disp) sys
aluAccumOp (ALUreg8 (IYindirect disp)) arithOp nFlag sys = doALUAccumVal (arithOp memVal) nFlag sys'
  where
    (memVal, sys') = sysMRead (reg16get IY sys + signExtend disp) sys
aluAccumOp (ALUreg8 reg)   arithOp nFlag sys             = doALUAccumReg8 (z80Reg8Lens reg) arithOp nFlag sys


doALUAccumReg8
  :: Lens Z80registers Z80registers Z80word Z80word
  -> (Z80word -> Z80word -> Z80word)
  -> Bool
  -> Z80system sysTag
  -> Z80system sysTag
doALUAccumReg8 regLens arithOp nFlag sys =
  arithFlags accumVal newAccum nFlag (sys & processor . cpu . regs . z80accum .~ newAccum)
  where
    accumVal = z80registers sys ^. z80accum
    newAccum = accumVal `arithOp` (z80registers sys ^. regLens)
    
doALUAccumVal
  :: (Z80word -> Z80word)
  -> Bool
  -> Z80system sysTag
  -> Z80system sysTag
doALUAccumVal arithOp nFlag sys =
  arithFlags accumVal newAccum nFlag (sys & processor . cpu . regs . z80accum .~ newAccum)
  where
    accumVal = z80registers sys ^. z80accum
    newAccum = arithOp accumVal
