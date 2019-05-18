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


aluAccumOp
  :: OperALU
  -> (Z80word -> Z80word -> Z80word)
  -> Bool
  -> Z80system sysTag
  -> Z80system sysTag
aluAccumOp (ALUreg8 A)   arithOp nFlag sys = doALUAccumReg8 z80accum arithOp nFlag sys
aluAccumOp (ALUreg8 B)   arithOp nFlag sys = doALUAccumReg8 z80breg  arithOp nFlag sys
aluAccumOp (ALUreg8 C)   arithOp nFlag sys = doALUAccumReg8 z80creg  arithOp nFlag sys
aluAccumOp (ALUreg8 D)   arithOp nFlag sys = doALUAccumReg8 z80dreg  arithOp nFlag sys
aluAccumOp (ALUreg8 E)   arithOp nFlag sys = doALUAccumReg8 z80ereg  arithOp nFlag sys
aluAccumOp (ALUreg8 H)   arithOp nFlag sys = doALUAccumReg8 z80hreg  arithOp nFlag sys
aluAccumOp (ALUreg8 L)   arithOp nFlag sys = doALUAccumReg8 z80lreg  arithOp nFlag sys
aluAccumOp (ALUreg8 IXh) arithOp nFlag sys = doALUAccumReg8 z80ixh   arithOp nFlag sys
aluAccumOp (ALUreg8 IXl) arithOp nFlag sys = doALUAccumReg8 z80ixl   arithOp nFlag sys
aluAccumOp (ALUreg8 IYh) arithOp nFlag sys = doALUAccumReg8 z80iyh   arithOp nFlag sys
aluAccumOp (ALUreg8 IYl) arithOp nFlag sys = doALUAccumReg8 z80iyl   arithOp nFlag sys
aluAccumOp (ALUreg8 HLindirect) arithOp nFlag sys = doALUAccumVal (arithOp memVal) nFlag sys'
  where
    (memVal, sys') = sysMRead (reg16get HL sys) sys
aluAccumOp (ALUreg8 (IXindirect disp)) arithOp nFlag sys = doALUAccumVal (arithOp memVal) nFlag sys'
  where
    (memVal, sys') = sysMRead (reg16get IX sys + signExtend disp) sys
aluAccumOp (ALUreg8 (IYindirect disp)) arithOp nFlag sys = doALUAccumVal (arithOp memVal) nFlag sys'
  where
    (memVal, sys') = sysMRead (reg16get IY sys + signExtend disp) sys
aluAccumOp (ALUimm  imm) arithOp nFlag sys = doALUAccumVal (arithOp imm) nFlag sys

doALUAccumReg8
  :: Lens Z80registers Z80registers Z80word Z80word
  -> (Z80word -> Z80word -> Z80word)
  -> Bool
  -> Z80system sysTag
  -> Z80system sysTag
doALUAccumReg8 lens arithOp nFlag sys =
  arithFlags accumVal newAccum nFlag (sys & processor . cpu . regs . z80accum .~ newAccum)
  where
    accumVal = z80registers sys ^. z80accum
    newAccum = accumVal `arithOp` (z80registers sys ^. lens)
    
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
