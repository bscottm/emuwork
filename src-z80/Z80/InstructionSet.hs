{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | The Haskell representation of the Z80 instruction set
module Z80.InstructionSet
  ( -- * Types
    Z80instruction(..)
  , Z80condC(..)
  , Z80reg8(..)
  , Z80reg16(..)
  , Reg8Reg8(..)
  , Reg8Imm(..)
  , AMemXfer(..)
  , Reg16Mem(..)
  , Reg16Imm(..)
  , AccumSpecials(..)
  , OperALU(..)
  , DestALUAcc(..)
  , DestALU16(..)
  , OperIO(..)
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
  )
where

import           Lens.Micro.Platform
import           Data.Data
import           Data.Int
import           Data.Map                       ( Map , (!))
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Generics.SOP.TH                ( deriveGeneric )

import           Machine
import           Z80.Processor

-- | The Z80 instruction set
data Z80instruction where
  -- Undefined/invalid instruction
  Z80undef :: [Z80word] -> Z80instruction
  -- Unified load/store (16-bit imm, 16-bit indirect)
  LDAspecial :: AccumSpecials -> Z80instruction
  LDr8r8 :: Reg8Reg8 -> Z80instruction
  LDr8imm :: Reg8Imm -> Z80instruction
  LDAmem :: AMemXfer -> Z80instruction
  LDr16mem :: Reg16Mem -> Z80instruction
  LDr16imm :: Reg16Imm -> Z80instruction
  -- Increment/decrement registers
  INC :: Z80reg8 -> Z80instruction
  DEC :: Z80reg8 -> Z80instruction
  INC16 :: RegPairSP -> Z80instruction
  DEC16 :: RegPairSP -> Z80instruction
  --- ALU group: AND, XOR, OR and CP (all are single operand)
  AND :: OperALU -> Z80instruction
  XOR :: OperALU -> Z80instruction
  OR :: OperALU -> Z80instruction
  CP :: OperALU -> Z80instruction
  -- 8-bit ALU vs. 16-bit ALU: ADD A, <something> vs. ADD16 (HL|IX|IY), rp
  ADD8 :: DestALUAcc -> Z80instruction
  ADD16 :: DestALU16 -> RegPairSP -> Z80instruction
  ADC8 :: DestALUAcc -> Z80instruction
  ADC16 :: DestALU16 -> RegPairSP -> Z80instruction
  SUB8 :: DestALUAcc -> Z80instruction
  SBC8 :: DestALUAcc -> Z80instruction
  SBC16 :: DestALU16 -> RegPairSP -> Z80instruction
  -- HALT; NOP; Exchanges; DI; EI; JP HL; LD SP, HL
  HALT :: Z80instruction
  NOP :: Z80instruction
  DI :: Z80instruction
  EI :: Z80instruction
  JPHL :: Z80instruction
  JPIX :: Z80instruction
  JPIY :: Z80instruction
  LDSPHL :: Z80instruction
  LDSPIX :: Z80instruction
  LDSPIY :: Z80instruction
  -- Exchanges:
  EXC :: Z80ExchangeOper -> Z80instruction
  -- Accumulator ops: RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
  RLCA :: Z80instruction
  RRCA :: Z80instruction
  RLA :: Z80instruction
  RRA :: Z80instruction
  DAA :: Z80instruction
  CPL :: Z80instruction
  SCF :: Z80instruction
  CCF :: Z80instruction
  -- Relative jumps: DJNZ and JR. Note: Even though these are relative jumps, the address is stored
  -- since it's easy to recompute the displacement.
  DJNZ :: (SymAbsAddr Z80addr) -> Z80instruction
  JR :: (SymAbsAddr Z80addr) -> Z80instruction
  JRCC :: Z80condC -> (SymAbsAddr Z80addr) -> Z80instruction
  -- Jumps
  JP :: (SymAbsAddr Z80addr) -> Z80instruction
  JPCC :: Z80condC -> (SymAbsAddr Z80addr) -> Z80instruction
  -- I/O instructions
  IN :: OperIO -> Z80instruction
  OUT :: OperIO -> Z80instruction
  -- Subroutine call
  CALL :: (SymAbsAddr Z80addr) -> Z80instruction
  CALLCC :: Z80condC -> (SymAbsAddr Z80addr) -> Z80instruction
  -- Return
  RET :: Z80instruction
  RETCC :: Z80condC -> Z80instruction
  -- Push, pop
  PUSH :: RegPairAF -> Z80instruction
  POP :: RegPairAF -> Z80instruction
  -- Restart
  RST :: Z80word -> Z80instruction
  -- 0xcb prefix instructions:
  -- RLC, RRC, RL, RR, SLA, SRA, SLL, SRL, BIT, RES, SET
  RLC :: Z80reg8 -> Z80instruction
  RRC :: Z80reg8 -> Z80instruction
  RL :: Z80reg8 -> Z80instruction
  RR :: Z80reg8 -> Z80instruction
  SLA :: Z80reg8 -> Z80instruction
  SRA :: Z80reg8 -> Z80instruction
  SLL :: Z80reg8 -> Z80instruction
  SRL :: Z80reg8 -> Z80instruction
  BIT :: Z80word -> Z80reg8 -> Z80instruction
  RES :: Z80word -> Z80reg8 -> Z80instruction
  SET :: Z80word -> Z80reg8 -> Z80instruction
  -- 0xcb prefix, undocumented IX- and IY-indexed, with result copied into an 8-bit register
  RLCidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  RRCidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  RLidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  RRidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  SLAidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  SRAidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  SLLidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  SRLidx :: Z80reg8 -> Z80reg8 -> Z80instruction
  RESidx :: Z80word -> Z80reg8 -> Z80reg8 -> Z80instruction
  SETidx :: Z80word -> Z80reg8 -> Z80reg8 -> Z80instruction
  -- 0xed prefix instructions:
  -- Negate accumulator
  NEG :: Z80instruction
  -- RETI, RETN: Return from interrupt, non-maskable interrupt
  RETI :: Z80instruction
  RETN :: Z80instruction
  -- Interrupt mode
  IM :: Z80word -> Z80instruction
  -- Rotate right/left, decimal
  RRD :: Z80instruction
  RLD :: Z80instruction
  -- Increment, Increment-Repeat instructions
  LDI :: Z80instruction
  CPI :: Z80instruction
  INI :: Z80instruction
  OUTI :: Z80instruction
  LDD :: Z80instruction
  CPD :: Z80instruction
  IND :: Z80instruction
  OUTD :: Z80instruction
  LDIR :: Z80instruction
  CPIR :: Z80instruction
  INIR :: Z80instruction
  OTIR :: Z80instruction
  LDDR :: Z80instruction
  CPDR :: Z80instruction
  INDR :: Z80instruction
  OTDR :: Z80instruction
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | 8-bit register to 8-bit register operand
data Reg8Reg8 where
  Reg8Reg8 :: Z80reg8 -> Z80reg8 -> Reg8Reg8
  deriving (Show, Eq, Ord, Typeable, Data)

-- | Immediate to 8-bit register. This isn't reversed; it follows the Z80 instruction
-- syntax, i.e., dest followed by source.
data Reg8Imm where
  Reg8Imm :: Z80reg8 -> Z80word -> Reg8Imm
  deriving (Show, Eq, Ord, Typeable, Data)

-- | Accumulator loads/store indirectly from/to (BC), (DE) or to/from from memory
data AMemXfer where
  FromBCindirect :: AMemXfer
  FromDEindirect :: AMemXfer
  ToBCindirect :: AMemXfer
  ToDEindirect :: AMemXfer
  AccFromMem :: SymAbsAddr Z80addr -> AMemXfer
  AccToMem :: SymAbsAddr Z80addr -> AMemXfer
  deriving (Show, Eq, Ord, Typeable, Data)

-- | 16-bit memory loads and stores, e.g. LD [BC|DE|HL|IX|IY|SP], (4000H)
-- Load [BC|DE|HL|IX|IY|SP] from the contents of 0x4000.
data Reg16Mem where
  ToReg16   :: RegPairSP -> (SymAbsAddr Z80addr) -> Reg16Mem
  FromReg16 :: RegPairSP -> (SymAbsAddr Z80addr) -> Reg16Mem
  deriving (Show, Eq, Ord, Typeable, Data)

-- 16-bit immediate load, e.g., LD BC, 4000H
data Reg16Imm where
  Reg16Imm :: RegPairSP -> (SymAbsAddr Z80addr) -> Reg16Imm
  deriving (Show, Eq, Ord, Typeable, Data)

-- | Accumulator <-> special registers
data AccumSpecials where
  -- A, I
  FromItoA :: AccumSpecials
  -- A, R
  FromRtoA :: AccumSpecials
  -- I, A
  FromAtoI :: AccumSpecials
  -- R, A
  FromAtoR :: AccumSpecials
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | ALU operands
data OperALU where
  ALUimm :: Z80word -> OperALU
  ALUreg8 :: Z80reg8 -> OperALU
  deriving (Show, Eq, Ord, Typeable, Data)

-- | 8-bit ALU operands where the accumulator is the destination, as distinct
-- from 16-bit ALU operations.
newtype DestALUAcc where
  ALUAcc :: OperALU -> DestALUAcc
  deriving (Show, Eq, Ord, Typeable, Data)

-- | 16-bit ALU destination operands -- HL, IX or IY
data DestALU16 where
  DestHL :: DestALU16
  DestIX :: DestALU16
  DestIY :: DestALU16
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data OperIO where
  PortImm :: Z80word -> OperIO
  CIndIO :: Z80reg8 -> OperIO
  CIndIO0 :: OperIO
  deriving (Show, Eq, Ord, Typeable, Data)

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
  deriving (Show, Eq, Ord, Typeable, Data)

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
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | General purpose 16-bit register operands
data Z80reg16 where
  BC :: Z80reg16
  DE :: Z80reg16
  HL :: Z80reg16
  IX :: Z80reg16
  IY :: Z80reg16
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes SP (instead of AF)
data RegPairSP where
  RPair16 :: Z80reg16 -> RegPairSP
  SP :: RegPairSP
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes AF (instead of SP)
data RegPairAF where
  AFPair16 :: Z80reg16 -> RegPairAF
  AF :: RegPairAF
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Exchange instruction operands
data Z80ExchangeOper where
  AFAF' :: Z80ExchangeOper
  DEHL :: Z80ExchangeOper
  SPHL :: Z80ExchangeOper
  SPIX :: Z80ExchangeOper
  SPIY :: Z80ExchangeOper
  Primes :: Z80ExchangeOper
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Automagically generated lenses:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

makeLenses ''Z80reg8

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Generics:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

$(deriveGeneric ''Z80instruction)
$(deriveGeneric ''Reg8Reg8)
$(deriveGeneric ''Reg8Imm)
$(deriveGeneric ''AMemXfer)
$(deriveGeneric ''Reg16Mem)
$(deriveGeneric ''Reg16Imm)
$(deriveGeneric ''AccumSpecials)
$(deriveGeneric ''OperALU)
$(deriveGeneric ''DestALU16)
$(deriveGeneric ''OperIO)
$(deriveGeneric ''Z80condC)
$(deriveGeneric ''Z80reg8)
$(deriveGeneric ''Z80reg16)
$(deriveGeneric ''RegPairSP)
$(deriveGeneric ''RegPairAF)
$(deriveGeneric ''Z80ExchangeOper)

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
reg16NameMap = Map.fromList [("bc", BC), ("de", DE), ("hl", HL)]
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
  :: Z80reg8
  -> Lens Z80registers Z80registers Z80word Z80word
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
