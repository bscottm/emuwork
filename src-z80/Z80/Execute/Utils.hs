module Z80.Execute.Utils
  ( make16bit
  , reg8set
  , reg8get
  , reg16get
  , reg16set
  , arithFlags
  )
where

import Data.Bits
import Data.Vector.Unboxed (Vector, (!))
import Lens.Micro.Platform

import Machine
import Z80.InstructionSet
import Z80.Processor
import Z80.System

-- | Make a 16-bit word from a vector of two 8-bit bytes.
make16bit :: Vector Z80byte -> Z80addr
make16bit bytes  = fromIntegral (bytes ! 1) `shiftL` 8 .|. fromIntegral (bytes ! 0)

-- | Set an 8-bit register from a value, returning the updated Z80 system. 
reg8set :: Z80reg8
        -> (Z80byte, Z80system sysType)
        -> Z80system sysType
reg8set HLindirect (val, sys)        = sysMWrite (reg16get HL sys) val sys
reg8set (IXindirect disp) (val, sys) = sysMWrite (reg16get IX sys + signExtend disp) val sys
reg8set (IYindirect disp) (val, sys) = sysMWrite (reg16get IY sys + signExtend disp) val sys
reg8set dstReg (val, sys)            = sys & processor . cpu . regs . z80Reg8Lens dstReg .~ val

-- | Get the value of a register, returning the byte and the updated Z80 system (which should not have changed, but makes the
-- function's signature one that can be used with `state`.)
reg8get :: Z80system sysType
        -> Z80reg8
        -> (Z80byte, Z80system sysType)
reg8get sys HLindirect        = sysMRead (reg16get HL sys) sys
reg8get sys (IXindirect disp) = sysMRead (reg16get IX sys + signExtend disp) sys
reg8get sys (IYindirect disp) = sysMRead (reg16get IY sys + signExtend disp) sys
reg8get sys srcReg            = (sys ^. processor . cpu . regs . z80Reg8Lens srcReg, sys)

-- | Get a 16-bit register's value.
reg16get
  :: Z80reg16
  -> Z80system sysType
  -> Z80addr

reg16get reg16 sys = (fromIntegral (z80regs ^. higetter) `shiftL` 8) .|. fromIntegral (z80regs ^. logetter)
  where
    (higetter, logetter) = z80Reg16HighLow reg16
    z80regs = z80registers sys

-- | Set a 16-bit register's value. Note that this breaks the 16-bit value into high and low
-- parts, moving the results into their respective high and low portions of the 16-bit register.
reg16set
  :: Z80addr
  -> Z80reg16
  -> Z80system sysType
  -> Z80system sysType
reg16set val reg16 sys = sys & processor . cpu .~ z80'
  where
    (hisetter, losetter) = z80Reg16HighLow reg16
    z80 = sys ^. processor . cpu
    z80' = z80 & regs . hisetter .~ fromIntegral ((val `shiftR` 8) .&. 0xff)
               & regs . losetter .~ fromIntegral (val .&. 0xff)

-- | Set the processor's flags on the result of an arithmetic operation. @nflag@ tells us whether the operation
-- was an addition or subtraction.
arithFlags
  :: Z80byte
  -> Z80byte
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
arithFlags oldval newval nFlag sys =
  sys & processor . cpu .~
    ( sys ^. processor . cpu 
     & flagSign .~ (newval >= 0x7f)
     & flagZero .~ (newval == 0)
     & flagYFlag .~ (newval .&. 0x20 /= 0)
     & flagHalfCarry .~ ((((newval .&. 0xf) + (oldval .&. 0xf)) .&. 0x10) /= 0)
     & flagXFlag .~ (newval .&. 0x08 /= 0)
     & flagParOv .~ (oldval .&. 0x80 `xor` newval .&. 0x80 /= 0)
     & flagNFlag .~ nFlag
     & flagCarry .~ (newval >= 0xff - oldval)
    )
