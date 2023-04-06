{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | The Haskell representation of the Z80 instruction set
module Z80.InstructionSet
  ( -- * Types
    Z80instruction(..)
  , Z80operand(..)
  , Z80OpndUndef
  , Z80OpndLoad
  , Z80OpndLoad16
  , Z80OpndInc
  , Z80OpndALU
  , Z80OpndALU16
  , Z80OpndIO
  , Z80condC(..)
  , Z80reg8(..)
  , Z80reg16(..)
  , RegPairSP(..)
  , RegPairAF(..)
  , Z80ExchangeOper(..)

  -- * Other utilities
  , reg8NameMap
  , reg8NameToReg
  , reg16NameMap
  , reg16NameToReg
  , idxRegNameMap
  , idxRegNameToReg
  , specialRegNames
  , z80Reg8Lens
  , z80Reg16HighLow)
where

-- import           Data.Data                     (Data, Typeable)
import           Data.Generics.Uniplate.Direct
import           Data.Int
import           Data.Map                      (Map, (!))
import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import           Lens.Micro.Platform           (makeLenses)

import           Machine                       (AbstractAddr)

import           Z80.Processor                 (Z80addr, Z80byte, Z80registers, z80accum, z80breg, z80creg, z80dreg, z80ereg,
                                                z80hreg, z80ixh, z80ixl, z80iyh, z80iyl, z80lreg)

-- | The Z80 instruction set
data Z80instruction where
  -- Undefined/invalid instruction
  Z80undef                      :: Z80operand Z80OpndUndef -> Z80instruction
  -- 8-bit loads and stores
  LD                            :: Z80operand Z80OpndLoad -> Z80instruction
  -- 16-bit loads and stores
  LD16                          :: Z80operand Z80OpndLoad16 -> Z80instruction
  -- Increment/decrement registers
  INC                           :: Z80operand Z80OpndInc -> Z80instruction
  DEC                           :: Z80operand Z80OpndInc -> Z80instruction
  -- ALU group                  : AND, XOR, OR and CP (all are single operand). SUB is an oddity
  -- becase it has the accumulator as an operand. ADD, ADC, SUB, SBC unify the
  -- unified 8-bit and 16-bit operands.
  AND                           :: Z80operand Z80OpndALU -> Z80instruction
  XOR                           :: Z80operand Z80OpndALU -> Z80instruction
  OR                            :: Z80operand Z80OpndALU -> Z80instruction
  CP                            :: Z80operand Z80OpndALU -> Z80instruction
  ADD                           :: Z80operand Z80OpndALU -> Z80instruction
  ADC                           :: Z80operand Z80OpndALU -> Z80instruction
  SUB                           :: Z80operand Z80OpndALU -> Z80instruction
  SBC                           :: Z80operand Z80OpndALU -> Z80instruction
  -- 16-bit ALU
  ADD16                         :: Z80operand Z80OpndALU16 -> Z80instruction
  ADC16                         :: Z80operand Z80OpndALU16 -> Z80instruction
  SBC16                         :: Z80operand Z80OpndALU16 -> Z80instruction
  -- HALT; NOP; Exchanges; DI; EI; JP HL; LD SP, HL
  HALT                          :: Z80instruction
  NOP                           :: Z80instruction
  DI                            :: Z80instruction
  EI                            :: Z80instruction
  JPHL                          :: Z80instruction
  JPIX                          :: Z80instruction
  JPIY                          :: Z80instruction
  LDSPHL                        :: Z80instruction
  LDSPIX                        :: Z80instruction
  LDSPIY                        :: Z80instruction
  -- Exchanges:
  EXC                           :: Z80ExchangeOper -> Z80instruction
  -- Accumulator ops            : RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
  RLCA                          :: Z80instruction
  RRCA                          :: Z80instruction
  RLA                           :: Z80instruction
  RRA                           :: Z80instruction
  DAA                           :: Z80instruction
  CPL                           :: Z80instruction
  SCF                           :: Z80instruction
  CCF                           :: Z80instruction
  -- Relative jumps             : DJNZ and JR. Note: Even though these are relative jumps, the address is stored
  -- since it's easy to recompute the displacement.
  DJNZ                          :: (AbstractAddr Z80addr) -> Z80instruction
  JR                            :: (AbstractAddr Z80addr) -> Z80instruction
  JRCC                          :: Z80condC -> (AbstractAddr Z80addr) -> Z80instruction
  -- Jumps
  JP                            :: (AbstractAddr Z80addr) -> Z80instruction
  JPCC                          :: Z80condC -> (AbstractAddr Z80addr) -> Z80instruction
  -- I/O instructions
  IN                            :: Z80operand Z80OpndIO -> Z80instruction
  OUT                           :: Z80operand Z80OpndIO -> Z80instruction
  -- Subroutine call
  CALL                          :: (AbstractAddr Z80addr) -> Z80instruction
  CALLCC                        :: Z80condC -> (AbstractAddr Z80addr) -> Z80instruction
  -- Return
  RET                           :: Z80instruction
  RETCC                         :: Z80condC -> Z80instruction
  -- Push, pop
  PUSH                          :: RegPairAF -> Z80instruction
  POP                           :: RegPairAF -> Z80instruction
  -- Restart
  RST                           :: Z80byte -> Z80instruction
  -- 0xcb prefix instructions   : 
  -- RLC, RRC, RL, RR, SLA, SRA, SLL, SRL, BIT, RES, SET
  RLC                           :: Z80reg8 -> Z80instruction
  RRC                           :: Z80reg8 -> Z80instruction
  RL                            :: Z80reg8 -> Z80instruction
  RR                            :: Z80reg8 -> Z80instruction
  SLA                           :: Z80reg8 -> Z80instruction
  SRA                           :: Z80reg8 -> Z80instruction
  SLL                           :: Z80reg8 -> Z80instruction
  SRL                           :: Z80reg8 -> Z80instruction
  BIT                           :: Z80byte -> Z80reg8 -> Z80instruction
  RES                           :: Z80byte -> Z80reg8 -> Z80instruction
  SET                           :: Z80byte -> Z80reg8 -> Z80instruction
  -- 0xcb prefix, undocumented IX- and IY-indexed, with result copied into an 8-bit register
  RLCidx                        :: Z80reg8 -> Z80reg8 -> Z80instruction
  RRCidx                        :: Z80reg8 -> Z80reg8 -> Z80instruction
  RLidx                         :: Z80reg8 -> Z80reg8 -> Z80instruction
  RRidx                         :: Z80reg8 -> Z80reg8 -> Z80instruction
  SLAidx                        :: Z80reg8 -> Z80reg8 -> Z80instruction
  SRAidx                        :: Z80reg8 -> Z80reg8 -> Z80instruction
  SLLidx                        :: Z80reg8 -> Z80reg8 -> Z80instruction
  SRLidx                        :: Z80reg8 -> Z80reg8 -> Z80instruction
  RESidx                        :: Z80byte -> Z80reg8 -> Z80reg8 -> Z80instruction
  SETidx                        :: Z80byte -> Z80reg8 -> Z80reg8 -> Z80instruction
  -- 0xed prefix instructions   : 
  -- Negate accumulator
  NEG                           :: Z80instruction
  -- RETI, RETN                 : Return from interrupt, non-maskable interrupt
  RETI                          :: Z80instruction
  RETN                          :: Z80instruction
  -- Interrupt mode
  IM                            :: Z80byte -> Z80instruction
  -- Rotate right/left, decimal
  RRD                           :: Z80instruction
  RLD                           :: Z80instruction
  -- Increment, Increment-Repeat instructions
  LDI                           :: Z80instruction
  CPI                           :: Z80instruction
  INI                           :: Z80instruction
  OUTI                          :: Z80instruction
  LDD                           :: Z80instruction
  CPD                           :: Z80instruction
  IND                           :: Z80instruction
  OUTD                          :: Z80instruction
  LDIR                          :: Z80instruction
  CPIR                          :: Z80instruction
  INIR                          :: Z80instruction
  OTIR                          :: Z80instruction
  LDDR                          :: Z80instruction
  CPDR                          :: Z80instruction
  INDR                          :: Z80instruction
  OTDR                          :: Z80instruction
  deriving (Show, Eq)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Z80 instruction operands: Use GADTs to create a single operand type and
-- allow the instructions to constrain which operands they accept. The downside
-- is that we have to provide instances for Show, Eq, Uniplate, Biplate, ...
--
-- 'opndSig' is the operand constraint.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- Operand constraint types:
data Z80OpndUndef
data Z80OpndLoad
data Z80OpndLoad16
data Z80OpndALU
data Z80OpndALU16
data Z80OpndInc
data Z80OpndIO

data Z80operand opndSig where
  -- | Undefined instruction
  UndefInsn           :: [Z80byte] -> Z80operand Z80OpndUndef
  -- | 8-bit register to 8-bit register
  Reg8Reg8            :: Z80reg8 -> Z80reg8 -> Z80operand Z80OpndLoad
  -- | Immediate to 8-bit register. This isn't reversed; it follows the Z80 instruction
  -- syntax, i.e., dest followed by source.
  Reg8Imm             :: Z80reg8 -> Z80byte -> Z80operand Z80OpndLoad
  -- | Accumulator memory loads and stores
  FromBCindirect      :: Z80operand Z80OpndLoad
  FromDEindirect      :: Z80operand Z80OpndLoad
  ToBCindirect        :: Z80operand Z80OpndLoad
  ToDEindirect        :: Z80operand Z80OpndLoad
  AccFromMem          :: AbstractAddr Z80addr -> Z80operand Z80OpndLoad
  AccToMem            :: AbstractAddr Z80addr -> Z80operand Z80OpndLoad
  -- | 16-bit memory loads and stores, e.g. LD [BC|DE|HL|IX|IY|SP], (4000H)
  -- Load [BC|DE|HL|IX|IY|SP] from the contents of 0x4000.
  ToReg16             :: RegPairSP -> (AbstractAddr Z80addr) -> Z80operand Z80OpndLoad16
  FromReg16           :: RegPairSP -> (AbstractAddr Z80addr) -> Z80operand Z80OpndLoad16
  -- 16-bit immediate load, e.g., LD BC, 4000H
  Reg16Imm            :: RegPairSP -> (AbstractAddr Z80addr) -> Z80operand Z80OpndLoad
  -- | Accumulator <  -> special registers
  -- A, I
  FromItoA            :: Z80operand Z80OpndLoad
  -- A, R
  FromRtoA            :: Z80operand Z80OpndLoad
  -- I, A
  FromAtoI            :: Z80operand Z80OpndLoad
  -- R, A
  FromAtoR            :: Z80operand Z80OpndLoad
  -- | ALU operands   : Accumulator is always the destination. Zilog was inconsistent
  -- with the mnemonics -- ADC is "ADC A, val" whereas AND is "AND val".
  ALUimm              :: Z80byte -> Z80operand Z80OpndALU
  ALUreg8             :: Z80reg8 -> Z80operand Z80OpndALU
  -- | Increments
  IncDecReg8          :: Z80reg8 -> Z80operand Z80OpndInc
  IncDecReg16         :: RegPairSP -> Z80operand Z80OpndInc
  -- | 16-bit ALU destination operands -- HL, IX or IY
  DestHL :: RegPairSP -> Z80operand Z80OpndALU16
  DestIX :: RegPairSP -> Z80operand Z80OpndALU16
  DestIY :: RegPairSP -> Z80operand Z80OpndALU16
  -- | Port I/O
  PortImm :: Z80byte -> Z80operand Z80OpndIO
  CIndIO :: Z80reg8 -> Z80operand Z80OpndIO
  CIndIO0 :: Z80operand Z80OpndIO

instance Show (Z80operand opndSig) where
  show (UndefInsn bytes)        = (T.unpack "UndefInsn(" ++) $ shows bytes ")"
  show (Reg8Reg8 r1 r2)         = (T.unpack "Reg8Reg8(" ++) . shows r1 . commaSpace $ shows r2 ")"
  show (Reg8Imm reg imm)        = (T.unpack "Reg8Imm(" ++) . shows reg . commaSpace $ shows imm ")"
  show FromBCindirect           = T.unpack "FromBCIndirect"
  show FromDEindirect           = T.unpack "FromDEindirect"
  show ToBCindirect             = T.unpack "ToBCindirect"
  show ToDEindirect             = T.unpack "ToDEindirect"
  show (AccFromMem addr)        = (T.unpack "AccFromMem(" ++) $ shows addr ")"
  show (AccToMem addr)          = (T.unpack "AccToMem(" ++) $ shows addr ")"
  show (ToReg16 reg16 addr)     = (T.unpack "ToReg16(" ++) . shows reg16 . commaSpace $ shows addr ")"
  show (FromReg16 reg16 addr)   = (T.unpack "FromReg16(" ++) . shows reg16 . commaSpace $ shows addr ")"
  show (Reg16Imm reg16 imm)     = (T.unpack "Reg16Imm(" ++) . shows reg16 . commaSpace $ shows imm ")"
  show FromItoA                 = T.unpack "FromItoA"
  show FromRtoA                 = T.unpack "FromRtoA"
  show FromAtoI                 = T.unpack "FromAtoI"
  show FromAtoR                 = T.unpack "FromAtoR"
  show (ALUimm imm)             = (T.unpack "ALUimm(" ++) $ shows imm ")"
  show (ALUreg8 reg)            = (T.unpack "ALUreg8(" ++) $ shows reg ")"
  show (IncDecReg8 reg)         = (T.unpack "IncDecReg8(" ++) $ shows reg ")"
  show (IncDecReg16 reg)        = (T.unpack "IncDecReg16(" ++) $ shows reg ")"
  show (DestHL reg16)           = (T.unpack "DestHL(" ++) $ shows reg16 ")"
  show (DestIX reg16)           = (T.unpack "DestHL(" ++) $ shows reg16 ")"
  show (DestIY reg16)           = (T.unpack "DestHL(" ++) $ shows reg16 ")"
  show (PortImm imm)            = (T.unpack "Portimm(" ++) $ shows imm ")"
  show (CIndIO reg)             = (T.unpack "CIndIO(" ++) $ shows reg ")"
  show CIndIO0                  = T.unpack "CIndIO0"

commaSpace :: String -> String
commaSpace = (T.unpack ", " ++)

instance Eq (Z80operand opndSig) where
  (UndefInsn bytes) == (UndefInsn bytes') = bytes == bytes'
  (Reg8Reg8 r1 r2) == (Reg8Reg8 r1' r2') = (r1 == r1') && (r2 == r2')
  (Reg8Imm reg imm) == (Reg8Imm reg' imm') = (reg == reg') && (imm == imm')
  FromBCindirect == FromBCindirect = True
  FromDEindirect == FromDEindirect = True
  ToBCindirect == ToBCindirect = True
  ToDEindirect == ToDEindirect = True
  (AccFromMem addr) == (AccFromMem addr') = addr == addr'
  (AccToMem addr) == (AccToMem addr') = addr == addr'
  (ToReg16 reg16 addr) == (ToReg16 reg16' addr')= (reg16 == reg16') && (addr == addr')
  (FromReg16 reg16 addr) == (FromReg16 reg16' addr')= (reg16 == reg16') && (addr == addr')
  (Reg16Imm reg16 imm) == (Reg16Imm reg16' imm') = reg16 == reg16' && imm == imm'
  FromItoA == FromItoA = True
  FromRtoA == FromRtoA = True
  FromAtoI == FromAtoI = True
  FromAtoR == FromAtoR = True
  (ALUimm imm) == (ALUimm imm') = imm == imm'
  (ALUreg8 reg) == (ALUreg8 reg') = reg == reg'
  (IncDecReg8 reg) == (IncDecReg8 reg') = reg == reg'
  (IncDecReg16 reg) == (IncDecReg16 reg') = reg == reg'
  (DestHL reg16) == (DestHL reg16') = reg16 == reg16'
  (DestIX reg16) == (DestIX reg16') = reg16 == reg16'
  (DestIY reg16) == (DestIY reg16') = reg16 == reg16'
  (PortImm imm) == (PortImm imm') = imm == imm'
  (CIndIO reg) == (CIndIO reg') = reg == reg'
  CIndIO0 == CIndIO0 = True
  _ == _ = False

instance Uniplate (Z80operand opndSig) where
  uniplate = plate

instance Biplate (Z80operand opndSig) Z80reg8 where
  biplate (Reg8Reg8 r1 r2)        = plate Reg8Reg8 |* r1 |* r2
  biplate (Reg8Imm reg imm)       = plate Reg8Imm |* reg |- imm
  biplate (ALUreg8 reg)           = plate ALUreg8 |* reg
  biplate (IncDecReg8 reg)        = plate IncDecReg8 |* reg
  biplate (CIndIO reg)            = plate CIndIO |* reg
  biplate insn                    = plate insn

instance Biplate (Z80operand opndSig) (AbstractAddr Z80addr) where
  biplate (AccToMem addr)         = plate AccToMem |* addr
  biplate (AccFromMem addr)       = plate AccFromMem |* addr
  biplate (ToReg16 reg16 addr)    = plate ToReg16 |- reg16 |* addr
  biplate (FromReg16 reg16 addr)  = plate FromReg16 |- reg16 |* addr
  biplate (Reg16Imm reg16 addr)   = plate Reg16Imm |- reg16 |* addr
  biplate insn                    = plate insn

instance Biplate (Z80operand opndSig) RegPairSP where
  biplate (ToReg16 reg16 addr)    = plate ToReg16 |* reg16 |- addr
  biplate (FromReg16 reg16 addr)  = plate FromReg16 |* reg16 |- addr
  biplate (Reg16Imm reg16 imm)    = plate Reg16Imm |* reg16 |- imm
  biplate (IncDecReg16 reg)       = plate IncDecReg16 |* reg
  biplate (DestHL reg16)          = plate DestHL |* reg16
  biplate (DestIX reg16)          = plate DestIX |* reg16
  biplate (DestIY reg16)          = plate DestIY |* reg16
  biplate insn                    = plate insn

instance Biplate (Z80operand opndSig) (Z80operand opndSig) where
  biplate = plateSelf

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Z80 condition flags
-- (note: rethink the names?)
data Z80condC where
  NZ :: Z80condC
  Z :: Z80condC
  NC :: Z80condC
  CY :: Z80condC
  PO :: Z80condC
  PE :: Z80condC
  POS :: Z80condC
  MI :: Z80condC
  deriving (Show, Eq)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Z80 8-bit registers, as operands. This also includes '(HL)' and the indexed+displacement for symmetry.
data Z80reg8 where
  A :: Z80reg8                          -- Index 7
  B :: Z80reg8                          -- Index 0
  C :: Z80reg8                          -- Index 1
  D :: Z80reg8                          -- Index 2
  E :: Z80reg8                          -- Index 3
  H :: Z80reg8                          -- Index 4
  L :: Z80reg8                          -- Index 5
  HLindirect :: Z80reg8                 -- Index 6
  IXindirect :: Int8 -> Z80reg8         -- IX + byte displacement
  IYindirect :: Int8 -> Z80reg8         -- IY + byte displacement
  -- Z80 actually implements IX and IY as a pair of 8-bit registers. This means
  -- that the instruction decoding transforms can also load constants into these
  -- half-registers and other unworldly tricks involving the H and L registers.
  --
  -- Also, the I[XY][hl] transformation doesn't apply if an indexed IX+disp or
  -- IY+disp is used. So, LD IXh, (IX+2) doesn't exist.
  IXh :: Z80reg8
  IXl :: Z80reg8
  IYh :: Z80reg8
  IYl :: Z80reg8
  deriving (Show, Eq, Ord {-, Typeable, Data-})

instance Uniplate Z80reg8 where
  uniplate = plate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | General purpose 16-bit register operands
data Z80reg16 where
  BC :: Z80reg16
  DE :: Z80reg16
  HL :: Z80reg16
  IX :: Z80reg16
  IY :: Z80reg16
  deriving (Show, Eq)

instance Uniplate Z80reg16 where
  uniplate = plate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes SP (instead of AF)
data RegPairSP where
  RPair16 :: Z80reg16 -> RegPairSP
  SP :: RegPairSP
  deriving (Show, Eq)

instance Uniplate RegPairSP where
  uniplate = plate

instance Biplate RegPairSP Z80reg16 where
  biplate (RPair16 reg16) = plate RPair16 |* reg16
  biplate SP = plate SP

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes AF (instead of SP)
data RegPairAF where
  AFPair16 :: Z80reg16 -> RegPairAF
  AF :: RegPairAF
  deriving (Show, Eq)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Exchange instruction operands
data Z80ExchangeOper where
  AFAF' :: Z80ExchangeOper
  DEHL :: Z80ExchangeOper
  SPHL :: Z80ExchangeOper
  SPIX :: Z80ExchangeOper
  SPIY :: Z80ExchangeOper
  Primes :: Z80ExchangeOper
  deriving (Show, Eq)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Automagically generate lenses:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

$(makeLenses ''Z80reg8)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Register names
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Utility mapping from lower case register names to 'Z80reg8' data constructors
reg8NameMap :: Map T.Text Z80reg8
reg8NameMap = Map.fromList
  [ ("a", A)
  , ("b", B)
  , ("c", C)
  , ("d", D)
  , ("e", E)
  , ("h", H)
  , ("l", L)
  ]

-- | Convert 8-bit register name to 'Z80reg8' data constructor. This will call 'error' if the lookup fails.
reg8NameToReg :: T.Text -> Z80reg8
reg8NameToReg reg = reg8NameMap ! reg

reg16NameMap :: Map T.Text Z80reg16
reg16NameMap = Map.fromList
  [ ("bc", BC)
  , ("de", DE)
  , ("hl", HL)
  ]

-- | Convert regular 16-bit register name to 'Z80reg16' data constructor. This will call 'error' if the lookup fails.
reg16NameToReg :: T.Text -> Z80reg16
reg16NameToReg reg = reg16NameMap ! reg

idxRegNameMap :: Map T.Text Z80reg16
idxRegNameMap = Map.fromList [("ix", IX), ("iy", IY)]
-- | Convert index register name to 'Z80reg16' data constructor. This will call 'error' if the lookup fails.
idxRegNameToReg :: T.Text -> Z80reg16
idxRegNameToReg reg = idxRegNameMap ! reg

specialRegNames :: [T.Text]
specialRegNames = ["sp", "af", "i", "r"]

-- | 8-bit register to `Lens` translation. @note@: This does not include the indirect
-- memory references. It just can't.
z80Reg8Lens
  :: (Functor f1)
  => Z80reg8
  -> (Z80byte -> f1 Z80byte) -> Z80registers -> f1 Z80registers
z80Reg8Lens HLindirect     = error "z80Reg8Lens: Caught HLindirect"
z80Reg8Lens (IXindirect _) = error "z80Reg8Lens: Caught IXindirect"
z80Reg8Lens (IYindirect _) = error "z80Reg8Lens: Caught IYindirect"
z80Reg8Lens reg8           = case reg8 of
      A     -> z80accum
      B     -> z80breg
      C     -> z80creg
      D     -> z80dreg
      E     -> z80ereg
      H     -> z80hreg
      L     -> z80lreg
      IXh   -> z80ixh
      IXl   -> z80ixl
      IYh   -> z80iyh
      IYl   -> z80iyl

-- | Return the high and low lenses for a register pair.
--
-- Note: The type signature would make more sense if it could use Lens or Lens'.
-- Unfortunately, though, that results in impredicative types.
z80Reg16HighLow
  :: (Functor f1, Functor f2)
  => Z80reg16
  -> ((Z80byte -> f1 Z80byte) -> Z80registers -> f1 Z80registers,
      (Z80byte -> f2 Z80byte) -> Z80registers -> f2 Z80registers)
z80Reg16HighLow reg16 = case reg16 of
  BC -> (z80breg, z80creg)
  DE -> (z80dreg, z80ereg)
  HL -> (z80hreg, z80lreg)
  IX -> (z80ixh,  z80ixl)
  IY -> (z80iyh,  z80iyl)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Uniplate/Biplate instances. These are the minimal instances for needed IX and
-- IY transforms to function without type checking errors.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

instance Uniplate Z80instruction where
  uniplate = plate

instance Biplate Z80instruction Z80instruction where
  biplate = plateSelf

instance Biplate Z80instruction (Z80operand opndType) where
  biplate = plate

instance Biplate Z80instruction Z80reg8 where
  biplate (LD opnd) = plate LD |+ opnd
  biplate (AND opnd) = plate AND |+ opnd
  biplate (XOR opnd) = plate XOR |+ opnd
  biplate (OR opnd) = plate OR |+ opnd
  biplate (CP opnd) = plate CP |+ opnd
  biplate (ADD opnd) = plate ADD |+ opnd
  biplate (ADC opnd) = plate ADC |+ opnd
  biplate (SUB opnd) = plate SUB |+ opnd
  biplate (SBC opnd) = plate SBC |+ opnd
  biplate (RLC reg) = plate RLC |* reg
  biplate (RRC reg) = plate RRC |* reg
  biplate (RL reg) = plate RL |* reg
  biplate (RR reg) = plate RR |* reg
  biplate (SLA reg) = plate SLA |* reg
  biplate (SRA reg) = plate SRA |* reg
  biplate (SLL reg) = plate SLL |* reg
  biplate (SRL reg) = plate SRL |* reg
  biplate (BIT bit reg) = plate BIT |- bit |* reg
  biplate (RES bit reg) = plate RES |- bit |* reg
  biplate (SET bit reg) = plate SET |- bit |* reg
  biplate (RLCidx r1 r2) = plate RLCidx |* r1 |* r2
  biplate (RRCidx r1 r2) = plate RRCidx |* r1 |* r2
  biplate (RLidx r1 r2) = plate RLidx |* r1 |* r2
  biplate (RRidx r1 r2) = plate RRidx |* r1 |* r2
  biplate (SLAidx r1 r2) = plate SLAidx |* r1 |* r2
  biplate (SRAidx r1 r2) = plate SRAidx |* r1 |* r2
  biplate (SLLidx r1 r2) = plate SLLidx |* r1 |* r2
  biplate (SRLidx r1 r2) = plate SRLidx |* r1 |* r2
  biplate (RESidx bit rIdx r2) = plate RESidx |- bit |* rIdx |* r2
  biplate (SETidx bit rIdx r2) = plate SETidx |- bit |* rIdx |* r2
  biplate insn = plate insn

instance Biplate Z80instruction (AbstractAddr Z80addr) where
  biplate (LD addr) = plate LD |+ addr
  biplate (LD16 addr) = plate LD16 |+ addr
  biplate (DJNZ addr) = plate DJNZ |* addr
  biplate (JR addr) = plate JR |* addr
  biplate (JRCC cc addr) = plate JRCC |- cc |* addr
  biplate (JP addr) = plate JP |* addr
  biplate (JPCC cc addr) = plate JPCC |- cc |* addr
  biplate (CALL addr) = plate CALL |* addr
  biplate (CALLCC cc addr) = plate CALLCC |- cc |* addr
  biplate insn = plate insn

instance Biplate Z80reg8 Z80reg8 where
  biplate = plateSelf
