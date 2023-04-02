module Z80.Execute.LoadStore
 ( insLoad
 , insLoadReg8Reg8
 , insLoadReg8Imm
 , insLoadAMemXfer
 , insLoadReg16Mem
 , insLoadReg16Imm
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

insLoadReg8Reg8 :: Reg8Reg8
                -> Z80system sysType
                -> Z80system sysType
insLoadReg8Reg8 (Reg8Reg8 dst src) sys                  = reg8set dst $ reg8get sys src

insLoadReg8Imm :: Reg8Imm
               -> Z80system sysType
               -> Z80system sysType
insLoadReg8Imm (Reg8Imm dst imm) sys                   = reg8set dst (imm, sys)

insLoadAMemXfer :: AMemXfer
                -> Z80system sysType
                -> Z80system sysType
insLoadAMemXfer FromBCindirect sys              = reg8set A $ sysMRead (reg16get BC sys) sys
insLoadAMemXfer FromDEindirect sys              = reg8set A $ sysMRead (reg16get DE sys) sys
insLoadAMemXfer ToBCindirect sys                = sysMWrite (reg16get BC sys) (z80registers sys ^. z80accum) sys
insLoadAMemXfer ToDEindirect sys                = sysMWrite (reg16get DE sys) (z80registers sys ^. z80accum) sys
insLoadAMemXfer (AccFromMem (AbsAddr addr)) sys = reg8set A $ sysMRead addr sys
insLoadAMemXfer (AccFromMem (SymAddr _   )) _   = error "insLoad/AccImm16Indirect: SymAddr encountered."
insLoadAMemXfer (AccToMem (AbsAddr addr)) sys   = sysMWrite addr (z80registers sys ^. z80accum) sys
insLoadAMemXfer (AccToMem (SymAddr _   )) _     = error "insLoad/Imm16IndirectStore: SymAddr encountered."

insLoadReg16Mem
  :: Reg16Mem
  -> Z80system sysType
  -> Z80system sysType
insLoadReg16Mem (FromReg16 (RPair16 BC) (AbsAddr addr)) sys = doStoreReg16 addr z80breg z80creg sys
insLoadReg16Mem (FromReg16 (RPair16 DE) (AbsAddr addr)) sys = doStoreReg16 addr z80dreg z80ereg sys
insLoadReg16Mem (FromReg16 (RPair16 HL) (AbsAddr addr)) sys = doStoreReg16 addr z80hreg z80lreg sys
insLoadReg16Mem (FromReg16 (RPair16 IX) (AbsAddr addr)) sys = doStoreReg16 addr z80ixh  z80ixl  sys
insLoadReg16Mem (FromReg16 (RPair16 IY) (AbsAddr addr)) sys = doStoreReg16 addr z80iyh  z80iyl  sys
insLoadReg16Mem (FromReg16 SP           (AbsAddr addr)) sys = execState writeSeq sys
  where
    z80regs           = z80registers sys
    hi                = fromIntegral ((z80regs ^. z80sp `shiftR` 8) .&. 0xff)
    lo                = fromIntegral (z80regs ^. z80sp) .&. 0xff
    writeSeq          = sequence [stateSysMWrite addr lo, stateSysMWrite (1+ addr) hi]
insLoadReg16Mem (FromReg16 _            (SymAddr _   )) _   = error "insLoadReg16Mem: SymAddr encountered"

insLoadReg16Mem (ToReg16  (RPair16 BC) (AbsAddr addr)) sys = doLoadReg16 addr z80breg z80creg sys
insLoadReg16Mem (ToReg16  (RPair16 DE) (AbsAddr addr)) sys = doLoadReg16 addr z80dreg z80ereg sys
insLoadReg16Mem (ToReg16  (RPair16 HL) (AbsAddr addr)) sys = doLoadReg16 addr z80hreg z80lreg sys
insLoadReg16Mem (ToReg16  (RPair16 IX) (AbsAddr addr)) sys = doLoadReg16 addr z80ixh  z80ixl  sys
insLoadReg16Mem (ToReg16  (RPair16 IY) (AbsAddr addr)) sys = doLoadReg16 addr z80iyh  z80iyl sys
insLoadReg16Mem (ToReg16  SP           (AbsAddr addr)) sys = sys' & processor . cpu . regs .~ z80regs'
  where
    (val, sys')      = sysMReadN addr 2 sys
    z80regs'         = z80registers sys & z80sp .~ make16bit val
insLoadReg16Mem (ToReg16  _            (SymAddr _   )) _   = error "insLoadReg16Mem: SymAddr encountered"

insLoadReg16Imm
  :: Reg16Imm
  -> Z80system sysType
  -> Z80system sysTypeinsLoadReg16Imm
insLoadReg16Imm (Reg16Imm (RPair16 rp) (AbsAddr val))  sys = reg16set val rp sys
insLoadReg16Imm (Reg16Imm SP           (AbsAddr addr)) sys = sys & processor . cpu . regs . z80sp .~ addr
insLoadReg16Imm (Reg16Imm _            (SymAddr _  ))  _   = error "insLoad/RPair16ImmLoad: SymAddr encountered."

-- | Z80 load instruction, with all of its manifest operands
insLoad :: AccumSpecials
        -> Z80system sysType
        -> Z80system sysType
insLoad FromItoA sys                             = iRegRRegFlags $ reg8set A (z80registers sys ^. z80ipage, sys)
insLoad FromRtoA sys                             = iRegRRegFlags $ reg8set A (z80registers sys ^. z80rreg, sys)
insLoad FromAtoI sys                             = sys & processor . cpu . regs . z80ipage .~ z80registers sys ^. z80accum
insLoad FromAtoR sys                             = sys & processor . cpu . regs . z80rreg .~ z80registers sys ^. z80accum


doStoreReg16
  :: Z80addr
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Z80system sysType
  -> Z80system sysType
doStoreReg16 addr higetter logetter sys = execState (sequence writeSeq) sys
  where
    z80regs = z80registers sys
    writeSeq =
      [ stateSysMWrite addr      (z80regs ^. logetter)
      , stateSysMWrite (1+ addr) (z80regs ^. higetter)
      ]

doLoadReg16
  :: Z80addr
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Z80system sysType
  -> Z80system sysType
doLoadReg16 addr hisetter losetter sys = sys' & processor . cpu . regs .~ z80regs'
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
