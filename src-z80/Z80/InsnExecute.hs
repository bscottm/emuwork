module Z80.InsnExecute
  ( z80instructionExecute

  -- * Test harness exports
  , reg16get
  , reg16set
  , make16bit
  )
where

import qualified Control.Arrow as Arrow
import Control.Monad (sequence)
import           Control.Monad.Trans.State.Strict (execState)
import Data.Bits
import           Data.Vector.Unboxed            (Vector, (!))
import Lens.Micro.Platform

import Machine
import Z80.InstructionSet
import Z80.Processor
import Z80.System

-- import Debug.Trace
-- import Text.Printf

z80instructionExecute :: InstructionExecute Z80state Z80instruction Z80addr Z80word
z80instructionExecute insn sys =
  case insn ^. decodedInsn of
    Z80undef _ -> nextInsnPC sys
    LD opnd    -> nextInsnPC $ insLoad opnd sys
    -- | Back up the PC by one: HALT is basically an infinite loop until an interrupt occurs
    HALT       -> updatePC (insnPC - 1) sys
    _          -> nextInsnPC sys
  where
    insnPC = insn ^. decodedInsnPC
    nextInsnPC = updatePC insnPC
    updatePC addr sys' = sys' & processor . cpu . z80pc .~ addr

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

make16bit :: Vector Z80word -> Z80addr
make16bit bytes  = fromIntegral (bytes ! 1) `shiftL` 8 .|. fromIntegral (bytes ! 0)

reg8set :: Z80reg8
        -> (Z80word, Z80system sysType)
        -> Z80system sysType
reg8set dstReg (val, sys) =
  case dstReg of
    A               -> sys & procRegs . z80accum .~ val
    B               -> sys & procRegs . z80breg .~ val
    C               -> sys & procRegs . z80creg .~ val
    D               -> sys & procRegs . z80dreg .~ val
    E               -> sys & procRegs . z80ereg .~ val
    H               -> sys & procRegs . z80hreg .~ val
    L               -> sys & procRegs . z80lreg .~ val
    HLindirect      -> sysMWrite (reg16get HL sys) val sys
    IXindirect disp -> sysMWrite (reg16get IX sys + signExtend disp) val sys
    IYindirect disp -> sysMWrite (reg16get IY sys + signExtend disp) val sys
    IXh             -> sys & procRegs . z80ixh .~ val
    IXl             -> sys & procRegs . z80ixl .~ val
    IYh             -> sys & procRegs . z80iyh .~ val
    IYl             -> sys & procRegs . z80iyl .~ val
  where
    procRegs = processor . cpu . regs

reg8get :: Z80system sysType
        -> Z80reg8
        -> (Z80word, Z80system sysType)
reg8get sys srcReg =
  case srcReg of
    A               -> (sys ^. procRegs . z80accum, sys)
    B               -> (sys ^. procRegs . z80breg, sys)
    C               -> (sys ^. procRegs . z80creg, sys)
    D               -> (sys ^. procRegs . z80dreg, sys)
    E               -> (sys ^. procRegs . z80ereg, sys)
    H               -> (sys ^. procRegs . z80hreg, sys)
    L               -> (sys ^. procRegs . z80lreg, sys)
    HLindirect      -> sysMRead (reg16get HL sys) sys
    IXindirect disp -> sysMRead (reg16get IX sys + signExtend disp) sys
    IYindirect disp -> sysMRead (reg16get IY sys + signExtend disp) sys
    IXh             -> (sys ^. procRegs . z80ixh, sys)
    IXl             -> (sys ^. procRegs . z80ixl, sys)
    IYh             -> (sys ^. procRegs . z80iyh, sys)
    IYl             -> (sys ^. procRegs . z80iyl, sys)
  where
    procRegs = processor . cpu . regs

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
doReg16get higetter logetter sys = ((fromIntegral hi) `shiftL` 8) .|. (fromIntegral lo)
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

doIndirectStoreReg16
  :: Z80addr
  -> Getting Z80word Z80registers Z80word
  -> Getting Z80word Z80registers Z80word
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
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
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
