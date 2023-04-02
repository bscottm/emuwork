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
make16bit :: Vector Z80word -> Z80addr
make16bit bytes  = fromIntegral (bytes ! 1) `shiftL` 8 .|. fromIntegral (bytes ! 0)

-- | Set an 8-bit register from a value, returning the updated Z80 system. 
reg8set :: Z80reg8
        -> (Z80word, Z80system sysType)
        -> Z80system sysType
reg8set HLindirect (val, sys)        = sysMWrite (reg16get HL sys) val sys
reg8set (IXindirect disp) (val, sys) = sysMWrite (reg16get IX sys + signExtend disp) val sys
reg8set (IYindirect disp) (val, sys) = sysMWrite (reg16get IY sys + signExtend disp) val sys
reg8set dstReg (val, sys)            = sys & processor . cpu . regs . z80Reg8Lens dstReg .~ val

-- | Get the value of a register, returning the byte and the updated Z80 system (which should not have changed, but makes the
-- function's signature one that can be used with `state`.)
reg8get :: Z80system sysType
        -> Z80reg8
        -> (Z80word, Z80system sysType)
reg8get sys HLindirect        = sysMRead (reg16get HL sys) sys
reg8get sys (IXindirect disp) = sysMRead (reg16get IX sys + signExtend disp) sys
reg8get sys (IYindirect disp) = sysMRead (reg16get IY sys + signExtend disp) sys
reg8get sys srcReg            = (sys ^. processor . cpu . regs . z80Reg8Lens srcReg, sys)

reg16get
  :: Z80reg16
  -> Z80system sysType
  -> Z80addr

reg16get BC {-sys-} = doReg16get z80breg z80creg {-sys-}
reg16get DE {-sys-} = doReg16get z80dreg z80ereg {-sys-}
reg16get HL {-sys-} = doReg16get z80hreg z80lreg {-sys-}
reg16get IX {-sys-} = doReg16get z80ixh  z80ixl  {-sys-}
reg16get IY {-sys-} = doReg16get z80iyh  z80iyl  {-sys-}

doReg16get
  :: Getting Z80word Z80registers Z80word
  -> Getting Z80word Z80registers Z80word
  -> Z80system sysType
  -> Z80addr
doReg16get higetter logetter sys = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
  where
    z80regs = z80registers sys
    hi      = z80regs ^. higetter
    lo      = z80regs ^. logetter


reg16set
  :: Z80addr
  -> Z80reg16
  -> Z80system sysType
  -> Z80system sysType

reg16set val BC {-sys-} = doReg16set val z80breg z80creg {-sys-}
reg16set val DE {-sys-} = doReg16set val z80dreg z80ereg {-sys-}
reg16set val HL {-sys-} = doReg16set val z80hreg z80lreg {-sys-}
reg16set val IX {-sys-} = doReg16set val z80ixh  z80ixl  {-sys-}
reg16set val IY {-sys-} = doReg16set val z80iyh  z80iyl  {-sys-}

doReg16set
  :: Z80addr
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> Z80system sysType
  -> Z80system sysType
doReg16set val hisetter losetter sys = sys & processor . cpu .~ z80'
  where
    z80 = sys ^. processor . cpu
    z80' = z80 & regs . hisetter .~ (fromIntegral ((val `shiftR` 8) .&. 0xff) :: Z80word)
               & regs . losetter .~ (fromIntegral (val .&. 0xff) :: Z80word)


-- | Set the processor's flags on the result of an arithmetic operation. @nflag@ tells us whether the operation
-- was an addition or subtraction.
arithFlags
  :: Z80word
  -> Z80word
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
