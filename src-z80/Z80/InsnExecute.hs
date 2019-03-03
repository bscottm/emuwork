module Z80.InsnExecute
  ( z80instructionExecute
  )
where

import qualified Control.Arrow as Arrow
import Control.Monad (sequence)
import Data.Bits
import Data.List (last)
import Data.Functor.Identity (Identity)
import           Data.Vector.Unboxed            ((!))
import Lens.Micro

import Machine
import Z80.InstructionSet
import Z80.Processor
import Z80.System

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

insLoad :: OperLD
        -> Z80system sysType
        -> Z80system sysType
insLoad (Reg8Reg8 dst src) sys = reg8set sys' dst val
  where
    (val, sys') = reg8get sys src

insLoad (Reg8Imm dst imm) sys = reg8set sys dst imm

insLoad AccBCIndirect sys = reg8set sys' A val
  where
    (val, sys') = sysMRead (reg16get BC sys) sys

insLoad AccDEIndirect sys = reg8set sys' A val
  where
    (val, sys') = sysMRead (reg16get DE sys) sys

insLoad (AccImm16Indirect (AbsAddr addr)) sys = reg8set sys' A val
  where
    (val, sys') = sysMRead addr sys
insLoad (AccImm16Indirect (SymAddr _   )) _   = error "insLoad/AccImm16Indirect: SymAddr encountered."

insLoad AccIReg sys = reg8set sys A (sys ^. processor . cpu . ipage)
insLoad AccRReg sys = reg8set sys A (sys ^. processor . cpu . refresh)

insLoad BCIndirectStore sys = sysMWrite (reg16get BC sys) (sys ^. processor . cpu . regs . z80accum) sys
insLoad DEIndirectStore sys = sysMWrite (reg16get DE sys) (sys ^. processor . cpu . regs . z80accum) sys
insLoad (Imm16IndirectStore (AbsAddr addr)) sys = sysMWrite addr (sys ^. processor . cpu . regs . z80accum) sys
insLoad (Imm16IndirectStore (SymAddr _   )) _   = error "insLoad/Imm16IndirectStore: SymAddr encountered."

insLoad IRegAcc sys = sys & processor . cpu .~ z80'
  where
    z80 = sys ^. processor . cpu
    z80' = z80 & ipage .~ z80 ^. regs . z80accum
insLoad RRegAcc sys = sys & processor . cpu .~ z80'
  where
    z80 = sys ^. processor . cpu
    z80' = z80 & refresh .~ z80 ^. regs . z80accum

insLoad (RPair16ImmLoad (RPair16 rp) (AbsAddr val))  sys = reg16set val rp sys
insLoad (RPair16ImmLoad SP           (AbsAddr addr)) sys = sys & processor . cpu . sp .~ addr
insLoad (RPair16ImmLoad _            (SymAddr _  ))  _   = error "insLoad/RPair16ImmLoad: SymAddr encountered."

insLoad (RPIndirectStore (RPair16 BC) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80breg z80creg sys
insLoad (RPIndirectStore (RPair16 DE) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80dreg z80ereg sys
insLoad (RPIndirectStore (RPair16 HL) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80hreg z80lreg sys
insLoad (RPIndirectStore (RPair16 IX) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80ixh  z80ixl  sys
insLoad (RPIndirectStore (RPair16 IY) (AbsAddr addr)) sys = doIndirectStoreReg16 addr z80iyh  z80iyl sys
insLoad (RPIndirectStore SP           (AbsAddr addr)) sys =
  last $ sequence [sysMWrite storeAddr lo, sysMWrite (storeAddr + 1) hi] sys'
  where
    z80 = sys ^. processor . cpu
    (storeAddr, sys')  = Arrow.first make16bit $ sysMReadN addr 2 sys
    hi                 = (fromIntegral (z80 ^. sp) `shiftR` 8) .&. 0xff :: Z80word
    lo                 = (fromIntegral (z80 ^. sp) .&. 0xff) :: Z80word
insLoad (RPIndirectStore _            (SymAddr _   )) _   = error "insLoad/HLIndirectStore: SymAddr encountered"

insLoad (RPIndirectLoad  (RPair16 BC) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80breg z80creg sys
insLoad (RPIndirectLoad  (RPair16 DE) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80dreg z80ereg sys
insLoad (RPIndirectLoad  (RPair16 HL) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80hreg z80lreg sys
insLoad (RPIndirectLoad  (RPair16 IX) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80ixh  z80ixl  sys
insLoad (RPIndirectLoad  (RPair16 IY) (AbsAddr addr)) sys = doIndirectLoadReg16 addr z80iyh  z80iyl sys
insLoad (RPIndirectLoad  SP           (AbsAddr addr)) sys = sys'' & processor . cpu .~ z80'
  where
    z80              = sys ^. processor . cpu
    (loadAddr, sys') = Arrow.first make16bit $ sysMReadN addr 2 sys
    (val, sys'')     = sysMReadN loadAddr 2 sys'
    z80'             = z80 & sp .~ make16bit val

insLoad (RPIndirectLoad  _            (SymAddr _   )) _   = error "insLoad/HLIndirectLoad: SymAddr encountered"

insLoad _ sys = sys

make16bit bytes  = (fromIntegral (bytes ! 1)) `shiftL` 8 .|. (fromIntegral (bytes ! 0))

reg8set :: Z80system sysType
        -> Z80reg8
        -> Z80word
        -> Z80system sysType
reg8set sys dstReg val =
  case dstReg of
    A               -> sys & procRegs . z80accum .~ val
    B               -> sys & procRegs . z80breg .~ val
    C               -> sys & procRegs . z80creg .~ val
    D               -> sys & procRegs . z80dreg .~ val
    E               -> sys & procRegs . z80ereg .~ val
    H               -> sys & procRegs . z80hreg .~ val
    L               -> sys & procRegs . z80lreg .~ val
    HLindirect      -> sysMWrite (reg16get HL sys) val sys
    IXindirect disp -> sysMWrite ((reg16get IX sys) + signExtend disp) val sys
    IYindirect disp -> sysMWrite ((reg16get IY sys) + signExtend disp) val sys
    IXh             -> sys & procRegs . z80ixh .~ val
    IXl             -> sys & procRegs . z80ixl .~ val
    IYh             -> sys & procRegs . z80iyh .~ val
    IYl             -> sys & procRegs . z80iyl .~ val
  where
    procRegs = (processor . cpu . regs)

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
    IXindirect disp -> sysMRead ((reg16get IX sys) + signExtend disp) sys
    IYindirect disp -> sysMRead ((reg16get IY sys) + signExtend disp) sys
    IXh             -> (sys ^. procRegs . z80ixh, sys)
    IXl             -> (sys ^. procRegs . z80ixl, sys)
    IYh             -> (sys ^. procRegs . z80iyh, sys)
    IYl             -> (sys ^. procRegs . z80iyl, sys)
  where
    procRegs = (processor . cpu . regs)

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
  :: (Integral a1, Integral a2)
  => Getting a1 Z80registers a1
  -> Getting a2 Z80registers a2
  -> Z80system sysType
  -> Z80addr
doReg16get higetter logetter sys = ((fromIntegral hi :: Z80addr) `shiftL` 8) .|. (fromIntegral lo :: Z80addr)
  where
    z80regs = sys ^. processor . cpu . regs
    hi = z80regs ^. higetter
    lo = z80regs ^. logetter


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
  -> ((a2 -> Identity Z80word) -> Z80registers -> Identity Z80registers)
  -> ((a3 -> Identity Z80word) -> Z80registers -> Identity Z80registers)
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
  -> EmulatedSystem Z80state insnSet Z80addr Z80word
  -> EmulatedSystem Z80state insnSet Z80addr Z80word
doIndirectStoreReg16 addr higetter logetter sys =
  last $ sequence [ sysMWrite storeAddr (z80regs ^. logetter), sysMWrite (storeAddr + 1) (z80regs ^. higetter)] sys'
  where
    z80regs = sys ^. processor . cpu . regs
    (storeAddr, sys')  = Arrow.first make16bit $ sysMReadN addr 2 sys

doIndirectLoadReg16
  :: Z80addr
  -> ((a2 -> Identity Z80word) -> Z80registers -> Identity Z80registers)
  -> ((a3 -> Identity Z80word) -> Z80registers -> Identity Z80registers)
  -> EmulatedSystem Z80state insnSet Z80addr Z80word
  -> EmulatedSystem Z80state insnSet Z80addr Z80word
doIndirectLoadReg16 addr hisetter losetter sys = sys'' & processor . cpu . regs .~ z80regs'
  where
    z80regs          = sys ^. processor . cpu . regs
    (loadAddr, sys') = Arrow.first make16bit $ sysMReadN addr 2 sys
    (val, sys'')     = sysMReadN loadAddr 2 sys'
    z80regs'         = z80regs & hisetter .~ val ! 1
                               & losetter .~ val ! 0
