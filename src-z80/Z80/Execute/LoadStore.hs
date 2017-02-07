module Z80.Execute.LoadStore
 ( insLoad
 )
where

import qualified Control.Arrow as Arrow
import Control.Monad (sequence)
import           Control.Monad.Trans.State.Strict (execState)
import Data.Bits
import           Data.Vector.Unboxed            ((!))
import Lens.Micro.Platform

import Machine
import Z80.Execute.Utils
import Z80.InstructionSet
import Z80.Processor
import Z80.System

-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~
-- LD instruction:
-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~

-- | Z80 load instruction, with all of its manifest operands
insLoad :: OperLD
        -> Z80system sysType
        -> Z80system sysType

insLoad (Reg8Reg8 dst src) sys                  = reg8set dst $ reg8get sys src
insLoad (Reg8Imm dst imm) sys                   = reg8set dst (imm, sys)
insLoad AccBCIndirect sys                       = reg8set A $ sysMRead (reg16get BC sys) sys
insLoad AccDEIndirect sys                       = reg8set A $ sysMRead (reg16get DE sys) sys
insLoad (AccImm16Indirect (AbsAddr addr)) sys   = reg8set A $ sysMRead addr sys
insLoad (AccImm16Indirect (SymAddr _   )) _     = error "insLoad/AccImm16Indirect: SymAddr encountered."
insLoad AccIReg sys                             = iRegRRegFlags $ reg8set A (z80registers sys ^. z80ipage, sys)
insLoad AccRReg sys                             = iRegRRegFlags $ reg8set A (z80registers sys ^. z80rreg, sys)
insLoad BCIndirectStore sys                     = sysMWrite (reg16get BC sys) (z80registers sys ^. z80accum) sys
insLoad DEIndirectStore sys                     = sysMWrite (reg16get DE sys) (z80registers sys ^. z80accum) sys
insLoad (Imm16IndirectStore (AbsAddr addr)) sys = sysMWrite addr (z80registers sys ^. z80accum) sys
insLoad (Imm16IndirectStore (SymAddr _   )) _   = error "insLoad/Imm16IndirectStore: SymAddr encountered."
insLoad IRegAcc sys                             = sys & processor . cpu . regs . z80ipage .~ z80registers sys ^. z80accum
insLoad RRegAcc sys                             = sys & processor . cpu . regs . z80rreg .~ z80registers sys ^. z80accum

insLoad (RPair16ImmLoad (RPair16 rp) (AbsAddr val))  sys = reg16set val rp sys
insLoad (RPair16ImmLoad SP           (AbsAddr addr)) sys = sys & processor . cpu . regs . z80sp .~ addr
insLoad (RPair16ImmLoad _            (SymAddr _  ))  _   = error "insLoad/RPair16ImmLoad: SymAddr encountered."

insLoad (RPIndirectStore (RPair16 BC) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80breg z80creg sys
insLoad (RPIndirectStore (RPair16 DE) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80dreg z80ereg sys
insLoad (RPIndirectStore (RPair16 HL) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80hreg z80lreg sys
insLoad (RPIndirectStore (RPair16 IX) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80ixh  z80ixl  sys
insLoad (RPIndirectStore (RPair16 IY) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80iyh  z80iyl  sys
insLoad (RPIndirectStore SP           (AbsAddr addr)) sys = execState writeSeq sys'
  where
    z80regs           = z80registers sys
    (storeAddr, sys') = Arrow.first make16bit $ sysMReadN addr 2 sys
    hi                = (fromIntegral (z80regs ^. z80sp) `shiftR` 8) .&. 0xff :: Z80word
    lo                = (fromIntegral (z80regs ^. z80sp) .&. 0xff)            :: Z80word
    writeSeq          = sequence [stateSysMWrite storeAddr lo, stateSysMWrite (storeAddr + 1) hi]
insLoad (RPIndirectStore _            (SymAddr _   )) _   = error "insLoad/HLIndirectStore: SymAddr encountered"

insLoad (RPIndirectLoad  (RPair16 BC) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80breg z80creg sys
insLoad (RPIndirectLoad  (RPair16 DE) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80dreg z80ereg sys
insLoad (RPIndirectLoad  (RPair16 HL) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80hreg z80lreg sys
insLoad (RPIndirectLoad  (RPair16 IX) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80ixh  z80ixl  sys
insLoad (RPIndirectLoad  (RPair16 IY) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80iyh  z80iyl sys
insLoad (RPIndirectLoad  SP           (AbsAddr addr)) sys = sys' & processor . cpu . regs .~ z80regs'
  where
    (val, sys')      = sysMReadN addr 2 sys
    z80regs'         = z80registers sys & z80sp .~ make16bit val

insLoad (RPIndirectLoad  _            (SymAddr _   )) _   = error "insLoad/HLIndirectLoad: SymAddr encountered"

doIndirectStoreReg16
  :: Z80addr
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Z80system sysType
  -> Z80system sysType
doIndirectStoreReg16 addr higetter logetter sys =
  execState (sequence writeSeq) sys'
  where
    z80regs = z80registers sys
    (storeAddr, sys')  = Arrow.first make16bit $ sysMReadN addr 2 sys
    writeSeq = [stateSysMWrite storeAddr (z80regs ^. logetter), stateSysMWrite (storeAddr + 1) (z80regs ^. higetter)]

doIndirectLoadReg16
  :: Z80addr
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Z80system sysType
  -> Z80system sysType
doIndirectLoadReg16 addr hisetter losetter sys = sys' & processor . cpu . regs .~ z80regs'
  where
    z80regs          = z80registers sys
    (val, sys')      = sysMReadN addr 2 sys
    z80regs'         = z80regs & hisetter .~ val ! 1
                               & losetter .~ val ! 0

-- | Compute a byte's parity. Returns 'True' if parity is even (P flag gets set) or 'False' if parity is odd.
-- Adapted from (https://graphics.stanford.edu/~seander/bithacks.html#ParityNaive)
computeParity :: Z80word -> Bool
computeParity val = ((0x96 :: Z80word) `shiftR` fromIntegral ((val `xor` (val `shiftR` 4)) .&. 0xf)) .&. 1 == 0

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
