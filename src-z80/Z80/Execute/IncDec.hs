module Z80.Execute.IncDec
  ( insIncDec
  , insIncDec16
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

insIncDec
  :: Z80reg8
  -> (Z80word -> Z80word)
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
insIncDec A {-op nFlag sys-} = doIncDec z80accum z80accum {-op nFlag sys-}
insIncDec B {-op nFlag sys-} = doIncDec z80breg z80breg {-op nFlag sys-}
insIncDec C {-op nFlag sys-} = doIncDec z80creg z80creg {-op nFlag sys-}
insIncDec D {-op nFlag sys-} = doIncDec z80dreg z80dreg {-op nFlag sys-}
insIncDec E {-op nFlag sys-} = doIncDec z80ereg z80ereg {-op nFlag sys-}
insIncDec H {-op nFlag sys-} = doIncDec z80hreg z80hreg {-op nFlag sys-}
insIncDec L {-op nFlag sys-} = doIncDec z80lreg z80lreg {-op nFlag sys-}
insIncDec HLindirect {-op nFlag sys-} = indirectIncDec HL 0 {-op nFlag sys-}
insIncDec (IXindirect disp) {-op nFlag sys-} = indirectIncDec IX disp {-op nFlag sys-}
insIncDec (IYindirect disp) {-op nFlag sys-} = indirectIncDec IY disp {-op nFlag sys-}
insIncDec IXh {-op nFlag sys-} = doIncDec z80ixh z80ixh {-op nFlag sys-}
insIncDec IXl {-op nFlag sys-} = doIncDec z80ixl z80ixl {-op nFlag sys-}
insIncDec IYh {-op nFlag sys-} = doIncDec z80iyh z80iyh {-op nFlag sys-}
insIncDec IYl {-op nFlag sys-} = doIncDec z80iyl z80iyl {-op nFlag sys-}

insIncDec16
  :: RegPairSP
  -> (Z80word -> Z80word)
  -> Z80system sysType
  -> Z80system sysType
insIncDec16 (RPair16 BC) op sys = doIncDec16 z80breg z80breg z80creg z80creg op sys
insIncDec16 (RPair16 DE) op sys = doIncDec16 z80dreg z80dreg z80ereg z80ereg op sys
insIncDec16 (RPair16 HL) op sys = doIncDec16 z80hreg z80hreg z80lreg z80lreg op sys
insIncDec16 (RPair16 IX) op sys = doIncDec16 z80ixh  z80ixh  z80ixl  z80ixl  op sys
insIncDec16 (RPair16 IY) op sys = doIncDec16 z80iyh  z80iyh  z80iyl  z80iyl  op sys
insIncDec16 SP           op sys = sys & processor . cpu . regs . z80sp .~ (spHi .|. (fromIntegral spLo))
  where
    z80regs = z80registers sys
    hi      = (fromIntegral (z80regs ^. z80sp) `shiftR` 8) .&. 0xff :: Z80word
    lo      = (fromIntegral (z80regs ^. z80sp) .&. 0xff)            :: Z80word
    spLo    = op lo
    spHi    = (fromIntegral (if spLo <= 0xff - lo then hi else op hi) :: Z80addr) `shiftL` 8

doIncDec
  :: SimpleGetter Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> (Z80word -> Z80word)
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
doIncDec getLens setLens op nFlag sys =
  incDecFlags oldval newval nFlag sys & processor . cpu . regs . setLens .~ newval
  where
    oldval = z80registers sys ^. getLens
    newval = op oldval

indirectIncDec
  :: Z80reg16
  -> Int8
  -> (Z80word -> Z80word)
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
indirectIncDec HL _disp op nFlag sys = incDecFlags oldval newval nFlag $ sysMWrite addr newval readSys
  where
    addr = reg16get HL sys
    (oldval, readSys) = sysMRead addr sys
    newval = op oldval
indirectIncDec BC _disp _op _nFlag _sys = undefined
indirectIncDec DE _disp _op _nFlag _sys = undefined
indirectIncDec reg disp op nFlag sys = incDecFlags oldval newval nFlag $ sysMWrite addr newval readSys
  where
    addr = reg16get reg sys + signExtend disp
    (oldval, readSys) = sysMRead addr sys
    newval = op oldval

doIncDec16
    :: SimpleGetter Z80registers Z80word
    -> ASetter Z80registers Z80registers Z80word Z80word
    -> SimpleGetter Z80registers Z80word
    -> ASetter Z80registers Z80registers Z80word Z80word
    -> (Z80word -> Z80word)
    -> Z80system sysType
    -> Z80system sysType
doIncDec16 hiGetLens hiSetLens loGetLens loSetLens op sys =
  sys & processor . cpu . regs .~
    ( sysRegs 
      & loSetLens .~ loval
      & hiSetLens .~ hival
    )
  where
    sysRegs = z80registers sys
    oldLoVal = sysRegs ^. loGetLens
    oldHiVal = sysRegs ^. hiGetLens
    loval = op oldLoVal
    hival = if oldLoVal .&. 0x80 `xor` loval .&. 0x80 == 0
            then oldHiVal
            else op oldHiVal


-- | Set the processor's flags on the result of an increment/decrement operation. @nflag@ tells us whether the operation
-- was an increment (False) or decrement (True). The carry flag isn't modified.
incDecFlags
  :: Z80word
  -> Z80word
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
