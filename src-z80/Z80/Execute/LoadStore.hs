module Z80.Execute.LoadStore
 ( insLoad
 , insLoad16
 )
where

import           Control.Monad.Trans.State.Strict (execState)

import           Data.Bits                        (Bits (shiftR, xor, (.&.)))
import           Data.Vector.Unboxed              ((!))

import           Lens.Micro.Platform              (Lens, (&), (.~), (^.))

import           Machine

import           Z80.Execute.Utils
import           Z80.InstructionSet
import           Z80.Processor
import           Z80.System

-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~
-- LD instruction:
-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~

insLoad
  :: Z80operand Z80OpndLoad
  -> Z80system sysType
  -> Z80system sysType

insLoad (Reg8Reg8 dst src) sys                  = reg8set dst $ reg8get sys src
insLoad (Reg8Imm dst imm) sys                   = reg8set dst (imm, sys)

insLoad FromBCindirect sys                   = reg8set A $ sysMRead (reg16get BC sys) sys
insLoad FromDEindirect sys                   = reg8set A $ sysMRead (reg16get DE sys) sys
insLoad ToBCindirect sys                     = sysMWrite (reg16get BC sys) (z80registers sys ^. z80accum) sys
insLoad ToDEindirect sys                     = sysMWrite (reg16get DE sys) (z80registers sys ^. z80accum) sys

insLoad (AccFromMem (AbstractAddr addr _)) sys = reg8set A $ sysMRead addr sys
insLoad (AccToMem (AbstractAddr addr _)) sys   = sysMWrite addr (z80registers sys ^. z80accum) sys

insLoad (Reg16Imm (RPair16 rp) addr)  sys = reg16set (absAddr addr) rp sys
insLoad (Reg16Imm SP           addr) sys  = sys & processor . cpu . regs . z80sp .~ absAddr addr

insLoad FromItoA sys                             = iRegRRegFlags $ reg8set A (z80registers sys ^. z80ipage, sys)
insLoad FromRtoA sys                             = iRegRRegFlags $ reg8set A (z80registers sys ^. z80rreg, sys)
insLoad FromAtoI sys                             = sys & processor . cpu . regs . z80ipage .~ z80registers sys ^. z80accum
insLoad FromAtoR sys                             = sys & processor . cpu . regs . z80rreg .~ z80registers sys ^. z80accum

insLoad16
  :: Z80operand Z80OpndLoad16
  -> Z80system sysType
  -> Z80system sysType
insLoad16 (FromReg16 (RPair16 BC) addr) sys = doStoreReg16 addr z80breg z80creg sys
insLoad16 (FromReg16 (RPair16 DE) addr) sys = doStoreReg16 addr z80dreg z80ereg sys
insLoad16 (FromReg16 (RPair16 HL) addr) sys = doStoreReg16 addr z80hreg z80lreg sys
insLoad16 (FromReg16 (RPair16 IX) addr) sys = doStoreReg16 addr z80ixh  z80ixl  sys
insLoad16 (FromReg16 (RPair16 IY) addr) sys = doStoreReg16 addr z80iyh  z80iyl  sys
insLoad16 (FromReg16 SP           addr) sys = execState writeSeq sys
  where
    z80regs           = z80registers sys
    hi                = fromIntegral ((z80regs ^. z80sp `shiftR` 8) .&. 0xff)
    lo                = fromIntegral (z80regs ^. z80sp) .&. 0xff
    addr'             = absAddr addr
    writeSeq          = sequence [stateSysMWrite  addr' lo, stateSysMWrite (1+ addr') hi]

insLoad16 (ToReg16  (RPair16 BC) addr) sys = doLoadReg16 addr z80breg z80creg sys
insLoad16 (ToReg16  (RPair16 DE) addr) sys = doLoadReg16 addr z80dreg z80ereg sys
insLoad16 (ToReg16  (RPair16 HL) addr) sys = doLoadReg16 addr z80hreg z80lreg sys
insLoad16 (ToReg16  (RPair16 IX) addr) sys = doLoadReg16 addr z80ixh  z80ixl  sys
insLoad16 (ToReg16  (RPair16 IY) addr) sys = doLoadReg16 addr z80iyh  z80iyl sys
insLoad16 (ToReg16  SP           addr) sys = sys' & processor . cpu . regs .~ z80regs'
  where
    (val, sys')      = sysMReadN (absAddr addr) 2 sys
    z80regs'         = z80registers sys & z80sp .~ make16bit val


doStoreReg16
  :: AbstractAddr Z80addr
  -> Lens Z80registers Z80registers Z80byte Z80byte
  -> Lens Z80registers Z80registers Z80byte Z80byte
  -> Z80system sysType
  -> Z80system sysType
doStoreReg16 addr higetter logetter sys = execState (sequence writeSeq) sys
  where
    z80regs = z80registers sys
    writeSeq =
      [ stateSysMWrite (absAddr addr)      (z80regs ^. logetter)
      , stateSysMWrite (1 + absAddr addr)  (z80regs ^. higetter)
      ]

doLoadReg16
  :: AbstractAddr Z80addr
  -> Lens Z80registers Z80registers Z80byte Z80byte
  -> Lens Z80registers Z80registers Z80byte Z80byte
  -> Z80system sysType
  -> Z80system sysType
doLoadReg16 addr hisetter losetter sys = sys' & processor . cpu . regs .~ z80regs'
  where
    z80regs          = z80registers sys
    (val, sys')      = sysMReadN (absAddr addr) 2 sys
    z80regs'         = z80regs & hisetter .~ val ! 1
                               & losetter .~ val ! 0

-- | Compute a byte's parity. Returns 'True' if parity is even (P flag gets set) or 'False' if parity is odd.
-- Adapted from (https://graphics.stanford.edu/~seander/bithacks.html#ParityNaive)
computeParity :: Z80byte -> Bool
computeParity val = ((0x96 :: Z80byte) `shiftR` fromIntegral ((val `xor` (val `shiftR` 4)) .&. 0xf)) .&. 1 == 0

iRegRRegFlags :: Z80system sysType -> Z80system sysType
iRegRRegFlags sys = sys & processor . cpu .~
                    ( sys ^. processor . cpu
                        & flagSign .~ (acc .&. 0x80 /= 0)
                        & flagZero .~ (acc == 0)
                        & flagHalfCarry .~ False
                        & flagParOv .~ computeParity acc
                        & flagNFlag .~ False
                    )
  where
    acc = z80registers sys ^. z80accum
