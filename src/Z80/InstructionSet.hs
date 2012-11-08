{-# LANGUAGE GADTs #-}
-- | The Haskell representation of the Z80 instruction set
module Z80.InstructionSet
  ( Instruction(..)
  ) where

import Z80.Processor

-- | The Z80 instruction set
data Instruction where
  -- 8-bit load group:
  -- LD r, r'
  -- LD r, n
  -- LD r, (HL)
  -- LD r, (IX + d)
  -- LD r, (IY + d)
  LD8 :: OperLD8
       -> Instruction
  -- LD (HL), r
  -- LD (IX + d), r
  -- LD (IY + d), r
  ST8 :: OperST8 Z80reg8
      -> Instruction
  -- LD (HL), n
  -- LD (IX + d), n
  -- LD (IY + d), n
  ST8I :: OperST8 Z80word
       -> Instruction
  -- LD A, (BC)
  -- LD A, (DE)
  -- LD A, (nn)
  -- LD A, I
  -- LD A, R
  LDA :: OperLDA
      -> Instruction
  -- LD (BC), A
  -- LD (DE), A
  -- LD (nn), A
  -- LD I, A
  -- LD R, A
  STA :: OperLDA
      -> Instruction

-- | 8-bit load group operands
data OperLD8 where
  Reg8Reg8   :: Z80reg8
             -> Z80reg8
             -> OperLD8
  Reg8Imm    :: Z80reg8
             -> Z80word
             -> OperLD8
  HLIndLoad :: Z80reg8
             -> OperLD8
  IXIndLoad :: Z80reg8
             -> Z80word
             -> OperLD8
  IYIndLoad :: Z80reg8
             -> Z80word
             -> OperLD8

-- | 8-bit register store group operands
data OperST8 x where
  HLIndStore :: x
             -> OperST8 x
  IXIndStore :: x
             -> Z80word
             -> OperST8 x
  IYIndStore :: x
             -> Z80word
             -> OperST8 x

-- | Accumulator load/store specials
data OperLDA where
  BCIndirect    :: OperLDA
  DEIndirect    :: OperLDA
  Imm16Indirect :: OperLDA
  IReg          :: OperLDA
  RReg          :: OperLDA
                
-- | Z80 8-bit registers
data Z80reg8 where
  A :: Z80reg8
  B :: Z80reg8
  C :: Z80reg8
  D :: Z80reg8
  E :: Z80reg8
  H :: Z80reg8
  L :: Z80reg8

data Z80reg16 where
  BC :: Z80reg16
  DE :: Z80reg16
  HL :: Z80reg16
