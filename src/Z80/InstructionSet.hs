{-# LANGUAGE DeriveDataTypeable #-}

-- | The Haskell representation of the Z80 instruction set
module Z80.InstructionSet
  ( -- * Types
    Z80emulation
  , Z80system
  , Z80instruction(..)
  , Z80condC(..)
  , Z80reg8(..)
  , Z80reg16(..)
  , OperLD(..)
  , OperALU(..)
  , OperExtendedALU(..)
  , OperIO(..)
  , RegPairSP(..)
  , RegPairAF(..)
  , Z80ExchangeOper(..)

  -- * Index register transform functions
  , Z80reg8XForm
  , Z80reg16XForm
  , Z80indexTransform(..)
  , z80nullTransform
  , z80ixTransform
  , z80iyTransform

  -- * Lens functions
  , reg8XForm
  , reg16XForm

  -- * Other utilities
  , reg8Names
  , reg16Names
  , idxRegNames
  , specialRegNames
  ) where

import Control.Lens
import Data.Typeable
import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import Machine
import Z80.Processor

-- | Shorthand for the Z80\'s 'EmulatedProcessor' type. This is defined here because 'Z80instruction' is required
-- and would otherwise form a module import cycle.
type Z80emulation = EmulatedProcessor Z80state Z80addr Z80instruction

-- | Shorthand for a Z80 emulated system. All Z80 systems share this characteristic type.
type Z80system memSys = EmulatedSystem Z80state memSys Z80addr Z80word Z80instruction

-- | The Z80 instruction set
data Z80instruction where
  -- Undefined/invalid instruction
  Z80undef                                 :: [Z80word]
                                           -> Z80instruction

  -- Unified load/store (reg8/reg8, 16-bit imm, accumulator, 16-bit indirect)
  LD                                       :: OperLD
                                           -> Z80instruction

  -- Increment/decrement registers
  INC, DEC                                 :: Z80reg8
                                           -> Z80instruction
  INC16, DEC16                             :: RegPairSP
                                           -> Z80instruction
  --- ALU group: ADD, ADC, SUB, SBC, AND, XOR, OR and CP
  -- ADD HL, rp and ADC/SBC HL, rp
  SUB, AND, XOR, OR, CP                    :: OperALU
                                           -> Z80instruction

  ADD                                      :: OperExtendedALU
                                           -> Z80instruction
  ADC                                      :: OperExtendedALU
                                           -> Z80instruction
  SBC                                      :: OperExtendedALU
                                           -> Z80instruction

  -- HALT; NOP; Exchanges; DI; EI; JP HL; LD SP, HL
  HALT, NOP, DI, EI, JPHL, LDSPHL          :: Z80instruction
  -- Exchanges:
  EXC                                      :: Z80ExchangeOper
                                           -> Z80instruction
  -- Accumulator ops: RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
  RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF :: Z80instruction
  -- Relative jumps: DJNZ and JR. Note: Even though these are relative jumps, the address is stored
  -- since it's easy to recompute the displacement.
  DJNZ                                     :: SymAbsAddr Z80addr
                                           -> Z80instruction
  JR                                       :: SymAbsAddr Z80addr
                                           -> Z80instruction
  JRCC                                     :: Z80condC
                                           -> SymAbsAddr Z80addr
                                           -> Z80instruction
  -- Jumps
  JP                                       :: SymAbsAddr Z80addr
                                           -> Z80instruction
  JPCC                                     :: Z80condC
                                           -> SymAbsAddr Z80addr
                                           -> Z80instruction
  -- I/O instructions
  IN, OUT                                  :: OperIO
                                           -> Z80instruction
  -- Subroutine call
  CALL                                     :: SymAbsAddr Z80addr
                                           -> Z80instruction
  CALLCC                                   :: Z80condC
                                           -> SymAbsAddr Z80addr
                                           -> Z80instruction
  -- Return
  RET                                      :: Z80instruction
  RETCC                                    :: Z80condC
                                           -> Z80instruction
  -- Push, pop
  PUSH, POP                                :: RegPairAF
                                           -> Z80instruction
  -- Restart
  RST                                      :: Z80word
                                           -> Z80instruction

  -- 0xcb prefix instructions:
  -- RLC, RRC, RL, RR, SLA, SRA, SLL, SRL, BIT, RES, SET
  RLC, RRC, RL, RR, SLA, SRA, SLL, SRL     :: Z80reg8
                                           -> Z80instruction
  BIT, RES, SET                            :: Z80word                      -- Bit within byte to set/reset/test
                                           -> Z80reg8
                                           -> Z80instruction

  -- 0xed prefix instructions:
  -- Negate accumulator
  NEG                                      :: Z80instruction

  -- RETI, RETN: Return from interrupt, non-maskable interrupt
  RETI, RETN                               :: Z80instruction

  -- Interrupt mode
  IM                                       :: Z80word
                                           -> Z80instruction

  -- Rotate right/left, decimal
  RRD, RLD                                 :: Z80instruction

  -- Increment, Increment-Repeat instructions
  LDI, CPI, INI, OUTI, LDD, CPD, IND, OUTD, LDIR, CPIR, INIR, OTIR, LDDR, CPDR, INDR, OTDR :: Z80instruction

  deriving (Show, Typeable, Data)
        
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Unified load/store operands:
data OperLD where
  Reg8Reg8           :: Z80reg8
                     -> Z80reg8
                     -> OperLD
  Reg8Imm            :: Z80reg8
                     -> Z80word
                     -> OperLD
  HLIndLoad          :: Z80reg8
                     -> OperLD
  IXIndLoad          :: Z80reg8
                     -> Z80disp
                     -> OperLD
  IYIndLoad          :: Z80reg8
                     -> Z80disp
                     -> OperLD
  -- Load accumulator
  AccBCIndirect      :: OperLD
  AccDEIndirect      :: OperLD
  AccImm16Indirect   :: SymAbsAddr Z80addr
                     -> OperLD
  AccIReg            :: OperLD
  AccRReg            :: OperLD
  -- Store accumulator
  BCIndirectStore    :: OperLD
  DEIndirectStore    :: OperLD
  Imm16IndirectStore :: SymAbsAddr Z80addr
                     -> OperLD
  IRegAcc            :: OperLD
  RRegAcc            :: OperLD
  -- 16-bit immediate load
  RPair16ImmLoad     :: RegPairSP
                     -> SymAbsAddr Z80addr
                     -> OperLD
  -- HL indirect: (nn), HL and HL, (nn)
  HLIndirectStore    :: SymAbsAddr Z80addr
                     -> OperLD
  HLIndirectLoad     :: SymAbsAddr Z80addr
                     -> OperLD
  -- 16-bit indirect loads and stores, e.g. LD BC, (4000H) [load BC from the contents of 0x4000]
  RPIndirectLoad     :: RegPairSP
                     -> SymAbsAddr Z80addr
                     -> OperLD
  RPIndirectStore    :: RegPairSP
                     -> SymAbsAddr Z80addr
                     -> OperLD
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | ALU operands
data OperALU where
  ALUimm        :: Z80word
                -> OperALU
  ALUreg8       :: Z80reg8
                -> OperALU
  ALUHLindirect :: OperALU
  deriving (Show, Typeable, Data)

-- | ALU operations that can also extend to use HL and register pair
data OperExtendedALU where
  ALU8  :: OperALU
        -> OperExtendedALU
  ALU16 :: RegPairSP
        -> OperExtendedALU
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data OperIO where
  PortImm :: Z80word
          -> OperIO
  CIndIO  :: Z80reg8
          -> OperIO
  CIndIO0 :: OperIO
  deriving (Show, Typeable, Data)


-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data Z80condC where
  NZ  :: Z80condC
  Z   :: Z80condC
  NC  :: Z80condC
  CY  :: Z80condC
  PO  :: Z80condC
  PE  :: Z80condC
  POS :: Z80condC
  MI  :: Z80condC
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Z80 8-bit registers
data Z80reg8 where
  A          :: Z80reg8                         -- Index 7
  B          :: Z80reg8                         -- Index 0
  C          :: Z80reg8                         -- Index 1
  D          :: Z80reg8                         -- Index 2
  E          :: Z80reg8                         -- Index 3
  H          :: Z80reg8                         -- Index 4
  L          :: Z80reg8                         -- Index 5
  HLindirect :: Z80reg8                         -- Index 6
  IXindirect :: Z80disp                         -- IX + displacement
             -> Z80reg8
  IYindirect :: Z80disp                         -- IY + displacement
             -> Z80reg8
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data Z80reg16 where
  BC :: Z80reg16
  DE :: Z80reg16
  HL :: Z80reg16
  IX :: Z80reg16
  IY :: Z80reg16
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes SP (instead of AF)
data RegPairSP where
  RPair16 :: Z80reg16
          -> RegPairSP
  SP      :: RegPairSP
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes AF (instead of SP)
data RegPairAF where
  RPair16' :: Z80reg16
           -> RegPairAF
  AF       :: RegPairAF
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Exchange instruction arguments
data Z80ExchangeOper where
  AFAF'  :: Z80ExchangeOper     -- AF with AF'
  DEHL   :: Z80ExchangeOper     -- DE with HL
  SPHL   :: Z80ExchangeOper     -- SP with HL
  Primes :: Z80ExchangeOper     -- EXX (regular <-> primes)
  deriving (Show, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Register names
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

reg8Names :: Map T.Text Z80reg8
reg8Names = Map.fromList [ ("a", A)
                         , ("b", B)
                         , ("c", C)
                         , ("d", D)
                         , ("e", E)
                         , ("h", H)
                         , ("l", L)
                         , ("(hl)", HLindirect)
                         ]

reg16Names :: [T.Text]
reg16Names = [ "bc"
             , "de"
             , "hl"
             ]

idxRegNames :: [T.Text]
idxRegNames = [ "ix"
              , "iy"
              ]

specialRegNames :: [T.Text]
specialRegNames = [ "sp", "af", "i", "r" ]

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Index register transform functions:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Shorthand for 8-bit register transform
type Z80reg8XForm memSys = ( Z80memory memSys           -- The memory system
                             -> Z80PC                   -- Program counter
                             -> Z80reg8                 -- Register to be transformed
                             -> (Z80PC, Z80reg8)        -- Possibly incremented program counter, transformed register pair tuple
                           )

-- | Shorthand for 16-bit register transform
type Z80reg16XForm = (Z80reg16                  -- Register pair to be transformed
                      -> Z80reg16)              -- Resulting transformed register pair

-- | Transform the 8-bit register operand to the IX register and displacement, only
-- if the operand is indirect via HL
ixXFormReg8 :: Z80reg8XForm memSys
-- Use lens transformations to operate on the second tuple member
ixXFormReg8 z80mem  pc HLindirect    = _2 %~ (IXindirect . fromIntegral) $ memIncPCAndFetch z80mem pc
ixXFormReg8 _z80mem z80state operand = (z80state, operand)

-- | Transform the 8-bit register operand to the IY register and displacement, only
-- if the operand is indirect via HL
iyXFormReg8 :: Z80reg8XForm memSys
iyXFormReg8 z80mem  z80state HLindirect = let (procState, disp) = memFetchAndIncPC z80mem z80state
                                              disp' = IXindirect $ fromIntegral disp
                                          in  (procState, disp')
iyXFormReg8 _z80mem z80state operand    = (z80state, operand)

-- | Transform 16-bit register operands to an index register, only if HL happens
-- to be the destination. Used when decoding 0xdd prefixed instructions

ixXFormReg16 :: Z80reg16XForm
ixXFormReg16 HL    = IX
ixXFormReg16 other = other

-- | See 'ixXFormReg16' documentation -- this is for the IY register
iyXFormReg16 ::Z80reg16XForm
iyXFormReg16 HL    = IY
iyXFormReg16 other = other

-- | A collection of register transforms. Note that access to individual elements of
-- the record is mediated via 'Data.Label' lenses.
data Z80indexTransform memSys =
  Z80indexTransform
  { _reg8XForm  :: Z80reg8XForm memSys          -- Z80reg8 8-bit register transform
  , _reg16XForm :: Z80reg16XForm                -- RegPairSP and RegPairAF 16-bit register transform
  }

-- | Pass-through register transform: no transform required
z80nullTransform :: Z80indexTransform memSys
z80nullTransform = Z80indexTransform
                   { _reg8XForm = (\_z80mem z80state operand -> (z80state, operand))
                   , _reg16XForm = id
                   }

-- | HL -> IX register transform collection
z80ixTransform :: Z80indexTransform memSys
z80ixTransform = Z80indexTransform
                 { _reg8XForm = ixXFormReg8
                 , _reg16XForm = ixXFormReg16
                 }

-- | HL -> IY register transform collection
z80iyTransform :: Z80indexTransform memSys
z80iyTransform = Z80indexTransform
                 { _reg8XForm = iyXFormReg8
                 , _reg16XForm = iyXFormReg16
                 }

makeLenses ''Z80indexTransform
