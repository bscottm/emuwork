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
  , OperLD(..)
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
data Z80instruction =
  -- Undefined/invalid instruction
    Z80undef [Z80word]
  -- Unified load/store (reg8/reg8, 16-bit imm, accumulator, 16-bit indirect)
  | LD OperLD
  -- Increment/decrement registers
  | INC Z80reg8
  | DEC Z80reg8
  | INC16 RegPairSP
  | DEC16 RegPairSP
  --- ALU group: ADD, ADC, SUB, SBC, AND, XOR, OR and CP (all are single operand)
  | SUB OperALU
  | AND OperALU
  | XOR OperALU
  | OR OperALU
  | CP OperALU
  -- 8-bit ALU vs. 16-bit ALU: ADD A, <something> vs. ADD16 (HL|IX|IY), rp
  | ADD8 DestALUAcc
  | ADD16 DestALU16 RegPairSP
  | ADC8 DestALUAcc
  | ADC16 DestALU16 RegPairSP
  | SBC8 DestALUAcc
  | SBC16 DestALU16 RegPairSP

  -- HALT; NOP; Exchanges; DI; EI; JP HL; LD SP, HL
  | HALT
  | NOP
  | DI
  | EI
  | JPHL
  | JPIX
  | JPIY
  | LDSPHL
  | LDSPIX
  | LDSPIY
  -- Exchanges:
  | EXC Z80ExchangeOper
  -- Accumulator ops: RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
  | RLCA
  | RRCA
  | RLA
  | RRA
  | DAA
  | CPL
  | SCF
  | CCF
  -- Relative jumps: DJNZ and JR. Note: Even though these are relative jumps, the address is stored
  -- since it's easy to recompute the displacement.
  | DJNZ (SymAbsAddr Z80addr)
  | JR (SymAbsAddr Z80addr)
  | JRCC Z80condC (SymAbsAddr Z80addr)
  -- Jumps
  | JP (SymAbsAddr Z80addr)
  | JPCC Z80condC (SymAbsAddr Z80addr)
  -- I/O instructions
  | IN OperIO
  | OUT OperIO
  -- Subroutine call
  | CALL (SymAbsAddr Z80addr)
  | CALLCC Z80condC (SymAbsAddr Z80addr)
  -- Return
  | RET
  | RETCC Z80condC
  -- Push, pop
  | PUSH RegPairAF
  | POP RegPairAF
  -- Restart
  | RST Z80word
  -- 0xcb prefix instructions:
  -- RLC, RRC, RL, RR, SLA, SRA, SLL, SRL, BIT, RES, SET
  | RLC Z80reg8
  | RRC Z80reg8
  | RL Z80reg8
  | RR Z80reg8
  | SLA Z80reg8
  | SRA Z80reg8
  | SLL Z80reg8
  | SRL Z80reg8
  | BIT Z80word Z80reg8
  | RES Z80word Z80reg8
  | SET Z80word Z80reg8
  -- 0xcb prefix, undocumented IX- and IY-indexed, with result copied into an 8-bit register
  | RLCidx Z80reg8 Z80reg8
  | RRCidx Z80reg8 Z80reg8
  | RLidx Z80reg8 Z80reg8
  | RRidx Z80reg8 Z80reg8
  | SLAidx Z80reg8 Z80reg8
  | SRAidx Z80reg8 Z80reg8
  | SLLidx Z80reg8 Z80reg8
  | SRLidx Z80reg8 Z80reg8
  | RESidx Z80word Z80reg8 Z80reg8
  | SETidx Z80word Z80reg8 Z80reg8
  -- 0xed prefix instructions:
  -- Negate accumulator
  | NEG
  -- RETI, RETN: Return from interrupt, non-maskable interrupt
  | RETI
  | RETN
  -- Interrupt mode
  | IM Z80word
  -- Rotate right/left, decimal
  | RRD
  | RLD
  -- Increment, Increment-Repeat instructions
  | LDI
  | CPI
  | INI
  | OUTI
  | LDD
  | CPD
  | IND
  | OUTD
  | LDIR
  | CPIR
  | INIR
  | OTIR
  | LDDR
  | CPDR
  | INDR
  | OTDR
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Unified load/store operands:
data OperLD =
    Reg8Reg8 Z80reg8 Z80reg8
  | Reg8Imm  Z80reg8 Z80word
  -- Load accumulator
  | AccBCIndirect
  | AccDEIndirect
  | AccImm16Indirect (SymAbsAddr Z80addr)
  -- Store accumulator via (BC) or (DE) indirectly (as pointers). Note that (HL) and (IX|IY+d) are already
  -- handled in Z80reg8
  | BCIndirectStore
  | DEIndirectStore
  | Imm16IndirectStore (SymAbsAddr Z80addr)
  -- A, I
  | AccIReg
  -- A, R
  | AccRReg
  -- I, A
  | IRegAcc
  -- R, A
  | RRegAcc
  -- 16-bit immediate load, e.g., LD BC, 4000H
  | RPair16ImmLoad RegPairSP (SymAbsAddr Z80addr)
  -- 16-bit indirect loads and stores, e.g. LD [BC|DE|HL|IX|IY|SP], (4000H)
  -- Load [BC|DE|HL|IX|IY|SP] from the contents of 0x4000.
  | RPIndirectLoad RegPairSP (SymAbsAddr Z80addr)
  | RPIndirectStore RegPairSP (SymAbsAddr Z80addr)
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | ALU operands
data OperALU =
    ALUimm Z80word
  | ALUreg8 Z80reg8
  deriving (Show, Eq, Ord, Typeable, Data)

-- | 8-bit ALU operands where the accumulator is the destination, as distinct
-- from 16-bit ALU operations.
newtype DestALUAcc = ALUAcc OperALU
  deriving (Show, Eq, Ord, Typeable, Data)

-- | 16-bit ALU destination operands -- HL, IX or IY
data DestALU16 =
    DestHL
  | DestIX
  | DestIY
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data OperIO =
    PortImm Z80word
  | CIndIO Z80reg8
  | CIndIO0
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Z80 condition flags
-- (note: rethink the names?)
data Z80condC =
    NZ
  | Z
  | NC
  | CY
  | PO
  | PE
  | POS
  | MI
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Z80 8-bit registers, as operands. This also includes '(HL)' and the indexed+displacement for symmetry.
data Z80reg8 =
    A                          -- Index 7
  | B                          -- Index 0
  | C                          -- Index 1
  | D                          -- Index 2
  | E                          -- Index 3
  | H                          -- Index 4
  | L                          -- Index 5
  | HLindirect                 -- Index 6
  | IXindirect Int8            -- IX + byte displacement
  | IYindirect Int8            -- IY + byte displacement
  -- Z80 actually implements IX and IY as two 8-bit registers each. Which means
  -- that the instruction decoding transforms can also load constants into these
  -- half-registers and other unworldly tricks involving the H and L registers.
  | IXh
  | IXl
  | IYh
  | IYl
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | General purpose 16-bit register operands
data Z80reg16 =
    BC
  | DE
  | HL
  | IX
  | IY
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes SP (instead of AF)
data RegPairSP =
    RPair16 Z80reg16
  | SP
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes AF (instead of SP)
data RegPairAF =
    AFPair16 Z80reg16
  | AF
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Exchange instruction operands
data Z80ExchangeOper =
    AFAF'     -- AF with AF'
  | DEHL      -- DE with HL
  | SPHL      -- (SP) with HL
  | SPIX      -- (SP) with IX
  | SPIY      -- (SP) with IY
  | Primes    -- EXX (regular <-> primes)
  deriving (Show, Eq, Ord, Typeable, Data)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Automagically generated lenses:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

makeLenses ''Z80reg8

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Generics:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

$(deriveGeneric ''Z80instruction)
$(deriveGeneric ''OperLD)
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
reg8NameMap = Map.fromList [("a", A), ("b", B), ("c", C), ("d", D), ("e", E), ("h", H), ("l", L)]

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
z80Reg8Lens A    = z80accum
z80Reg8Lens B    = z80breg
z80Reg8Lens C    = z80creg
z80Reg8Lens D    = z80dreg
z80Reg8Lens E    = z80ereg
z80Reg8Lens H    = z80hreg
z80Reg8Lens L    = z80lreg
z80Reg8Lens IXh  = z80ixh
z80Reg8Lens IXl  = z80ixl
z80Reg8Lens IYh  = z80iyh
z80Reg8Lens IYl  = z80iyl
z80Reg8Lens _    = undefined
