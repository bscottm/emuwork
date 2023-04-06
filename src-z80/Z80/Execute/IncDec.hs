module Z80.Execute.IncDec
  ( insInc
  , insDec
  )
where

import Data.Bits
import Data.Int
import Lens.Micro.Platform

import Machine
import Z80.Execute.Utils
import Z80.InstructionSet
import Z80.Processor
import Z80.System

-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~
-- INC/DEC/INC16/DEC16 instructions:
-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~

insInc
  :: Z80operand Z80OpndInc
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
insInc (IncDecReg8 HLindirect) nFlag sys = indirectIncDec HL 0 nFlag sys
insInc (IncDecReg8 (IXindirect disp)) nFlag sys = indirectIncDec IX disp nFlag sys
insInc (IncDecReg8 (IYindirect disp)) nFlag sys = indirectIncDec IY disp nFlag sys
insInc (IncDecReg8 reg) nFlag sys = doIncDec (z80Reg8Lens reg) nFlag sys
insInc (IncDecReg16 SP) _nFlag sys = sys & processor . cpu . regs . z80sp .~ (spHi .|. spLo)
  where
    z80regs = z80registers sys
    hi      = (fromIntegral (z80regs ^. z80sp) `shiftR` 8) .&. 0xff :: Z80addr
    lo      = fromIntegral (z80regs ^. z80sp) .&. 0xff :: Z80addr
    spLo    = 1+ lo
    spHi    = (fromIntegral (if spLo <= 0xff - lo then hi else 1+ hi) :: Z80addr) `shiftL` 8
insInc (IncDecReg16 (RPair16 reg)) nFlag sys = sys & processor . cpu . regs .~
    ( sysRegs 
      & loSetter .~ loval
      & hiSetter .~ hival
    )
  where
    -- These are two different Lens applications: Setter uses one functor type
    -- Gett(er|ing) uses a different functor type.
    (hiSetter, loSetter) = z80Reg16HighLow reg
    (hiGetter, loGetter) = z80Reg16HighLow reg
    sysRegs = z80registers sys
    oldLoVal = sysRegs ^. loGetter
    oldHiVal = sysRegs ^. hiGetter
    loval = 1+ oldLoVal
    hival = if not (overflow nFlag)
            then oldHiVal
            else 1+ oldHiVal
    -- Overflow if increment
    overflow False = loval < oldLoVal
    -- Overflow if decrement
    overflow True  = loval > oldLoVal

doIncDec
  :: Lens Z80registers Z80registers Z80byte Z80byte
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
doIncDec regLens nFlag sys =
  incDecFlags oldval newval nFlag sys & processor . cpu . regs . regLens .~ newval
  where
    oldval = z80registers sys ^. regLens
    newval = 1+ oldval

indirectIncDec
  :: Z80reg16
  -> Int8
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
indirectIncDec HL _disp nFlag sys = incDecFlags oldval newval nFlag $ sysMWrite addr newval readSys
  where
    addr = reg16get HL sys
    (oldval, readSys) = sysMRead addr sys
    newval = 1+ oldval
indirectIncDec BC _disp _nFlag sys = sys
indirectIncDec DE _disp _nFlag sys = sys
indirectIncDec reg disp nFlag sys = incDecFlags oldval newval nFlag $ sysMWrite addr newval readSys
  where
    addr = reg16get reg sys + signExtend disp
    (oldval, readSys) = sysMRead addr sys
    newval = 1+ oldval

-- | Set the processor's flags on the result of an increment/decrement operation. @nflag@ tells us whether the operation
-- was an increment (False) or decrement (True). The carry flag isn't modified.
incDecFlags
  :: Z80byte
  -> Z80byte
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
incDecFlags oldval newval nFlag sys =
  sys & processor . cpu .~
    ( sys ^. processor . cpu 
     & flagSign .~ (newval >= 0x7f)
     & flagZero .~ (newval == 0)
     & flagYFlag .~ (newval .&. 0x20 /= 0)
     & flagHalfCarry .~ ((((newval .&. 0xf) + (oldval .&. 0xf)) .&. 0x10) /= 0)
     & flagXFlag .~ (newval .&. 0x08 /= 0)
     & flagParOv .~ (oldval .&. 0x80 `xor` newval .&. 0x80 /= 0)
     & flagNFlag .~ nFlag
    )

insDec
  :: Z80operand Z80OpndInc
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
insDec _ _ _ = undefined