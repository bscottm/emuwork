module Z80.Execute.ALUops
  ( aluAccumOp
  )
where

import Control.Arrow (arr)
import Lens.Micro.Platform ( (&), (.~), (^.), Lens )

import Machine
import Z80.Execute.Utils
import Z80.InstructionSet
import Z80.Processor
import Z80.System


-- | Perform an arithmetic operation, 'arithOp', on the Z80 accumulator. Lifts the
-- memory indirections (HL, (IX+d) and (IY+d)) into an arrow computation.
aluAccumOp
  :: Z80operand Z80OpndALU
  -> (Z80byte -> Z80byte -> Z80byte)
  -> Bool
  -> Z80system sysTag
  -> Z80system sysTag
aluAccumOp op arithOp nFlag sys = case op of
  ALUimm imm                -> doALUAccumVal arithOp nFlag (imm, sys)
  ALUreg8 HLindirect        -> arr (doALUAccumVal arithOp nFlag) $ sysMRead (reg16get HL sys) sys
  ALUreg8 (IXindirect disp) -> arr (doALUAccumVal arithOp nFlag) $ sysMRead (reg16get IX sys + signExtend disp) sys
  ALUreg8 (IYindirect disp) -> arr (doALUAccumVal arithOp nFlag) $ sysMRead (reg16get IY sys + signExtend disp) sys
  ALUreg8 reg               -> doALUAccumReg8 (z80Reg8Lens reg) arithOp nFlag sys

doALUAccumReg8
  :: Lens Z80registers Z80registers Z80byte Z80byte
  -> (Z80byte -> Z80byte -> Z80byte)
  -> Bool
  -> Z80system sysTag
  -> Z80system sysTag
doALUAccumReg8 regLens arithOp nFlag sys =
  arithFlags accumVal newAccum nFlag (sys & processor . cpu . regs . z80accum .~ newAccum)
  where
    accumVal = z80registers sys ^. z80accum
    newAccum = accumVal `arithOp` (z80registers sys ^. regLens)
    
doALUAccumVal
  :: (Z80byte -> Z80byte -> Z80byte)
  -> Bool
  -> (Z80byte, Z80system sysTag)
  -> Z80system sysTag
doALUAccumVal arithOp nFlag (value, sys) =
  arithFlags accumVal newAccum nFlag (sys & processor . cpu . regs . z80accum .~ newAccum)
  where
    accumVal = z80registers sys ^. z80accum
    newAccum = accumVal `arithOp` value
