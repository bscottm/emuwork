{-# LANGUAGE GADTs #-}
-- | The Haskell representation of the Z80 instruction set
module Z80.InstructionSet
  ( Instruction(..)
  , Z80condC(..)
  , Z80reg8(..)
  , Z80reg16(..)
  , OperLD8(..)
  , OperLDA(..)
  , OperALU(..)
  , RegPairSP(..)
  , RegPairAF(..)
  ) where

import Machine.Utils
import Z80.Processor

-- | The Z80 instruction set
data Instruction where
  -- Undefined/invalid instruction
  Z80undef :: [Z80word]
           -> Instruction
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
  -- LD rp, nn
  LD16 :: RegPairSP
       -> Z80addr
       -> Instruction
  -- LD (nn), HL
  -- LD HL, (nn)
  STHL :: Z80addr
       -> Instruction
  LDHL :: Z80addr
       -> Instruction
  -- INC/DEC BC
  -- INC/DEC DE
  -- INC/DEC HL
  -- INC/DEC SP
  INC, DEC :: Z80reg8
           -> Instruction
  INC16, DEC16 :: RegPairSP
               -> Instruction
  --- ALU group: ADD, ADC, SUB, SBC, AND, XOR, OR and CP
  -- ADD HL, rp here too.
  ADD, ADC, SUB, SBC, AND, XOR, OR, CP :: OperALU
                                       -> Instruction
  ADDHL :: RegPairSP
        -> Instruction
  -- HALT; NOP; EX AF, AF'; DI; EI; EXX; JP HL; LD SP, HL
  HALT, NOP, EXAFAF', EXDEHL, EXSPHL, DI, EI, EXX, JPHL, LDSPHL :: Instruction
  -- Accumulator ops: RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
  RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF :: Instruction
  -- Relative jumps: DJNZ and JR. Note: Even though these are relative jumps, the address is stored
  -- since it's easy to recompute the displacement.
  DJNZ, JR :: Z80addr
           -> Instruction
  JRCC :: Z80condC
       -> Z80addr
       -> Instruction
  -- Jumps
  JP :: Z80addr
     -> Instruction
  JPCC :: Z80condC
       -> Z80addr
       -> Instruction
  -- I/O instructions
  IN, OUT :: Z80word
          -> Instruction
  -- Subroutine call
  CALL :: Z80addr
       -> Instruction
  CALLCC :: Z80condC
         -> Z80addr
         -> Instruction
  -- Return
  RET :: Instruction
  RETCC :: Z80condC
        -> Instruction
  -- Push, pop
  PUSH, POP :: RegPairAF
            -> Instruction
  -- Restart
  RST :: Z80word
      -> Instruction

-- | Instruction -> String serialization
instance Show Instruction where
  show (Z80undef bytes) = "Z80undef " ++ (asHex bytes)

  show (LD8 x) = "LD8(" ++ (show x) ++ ")"
  show (LDA x) = "LDA(" ++ (show x) ++ ")"
  show (STA x) = "STA(" ++ (show x) ++ ")"

  show (LD16 rp imm) = "LD16(" ++ (show rp) ++ "," ++ (asHex imm) ++ ")"

  show (LDHL addr) = "LDHL(" ++ (asHex addr) ++ ")"
  show (STHL addr) = "STHL(" ++ (asHex addr) ++ ")"

  show HALT = "HALT"
  show NOP = "NOP"
  show EXAFAF' = "EXAFAF'"
  show EXSPHL = "EXSPHL"
  show EXDEHL = "EXDEHL"
  show DI = "DI"
  show EI = "EI"
  show EXX = "EXX"
  show JPHL = "JPHL"
  show LDSPHL = "LDSPHL"

  show RLCA = "RLCA"
  show RRCA = "RRCA"
  show RLA = "RLA"
  show RRA = "RRA"
  show DAA = "DAA"
  show CPL = "CPL"
  show SCF = "SCF"
  show CCF = "CCF"

  show (DJNZ addr) = "DJNZ(" ++ (asHex addr) ++ ")"
  show (JR addr) = "JR(" ++ (asHex addr) ++ ")"
  show (JRCC cc addr) = "JRCC(" ++ (show cc) ++ "," ++ (asHex addr) ++ ")"

  show (JP addr) = "JP(" ++ (asHex addr) ++ ")"
  show (JPCC cc addr) = "JPCC(" ++ (show cc) ++ "," ++ (show addr) ++ ")"

  show (ADD op) = "ADD(" ++ (show op) ++ ")"
  show (ADC op) = "ADC(" ++ (show op) ++ ")"
  show (SUB op) = "SUB(" ++ (show op) ++ ")"
  show (SBC op) = "SBC(" ++ (show op) ++ ")"
  show (AND op) = "AND(" ++ (show op) ++ ")"
  show (XOR op) = "XOR(" ++ (show op) ++ ")"
  show (OR op) = "OR(" ++ (show op) ++ ")"
  show (CP op) = "CP(" ++ (show op) ++ ")"
  show (ADDHL rp) = "ADDHL(" ++ (show rp) ++ ")"

  show (INC reg) = "INC(" ++ (show reg) ++ ")"
  show (DEC reg) = "DEC(" ++ (show reg) ++ ")"
  show (INC16 rp) = "INC16(" ++ (show rp) ++ ")"
  show (DEC16 rp) = "DEC16(" ++ (show rp) ++ ")"

  show (IN port) = "IN(" ++ (asHex port) ++ ")"
  show (OUT port) = "OUT(" ++ (asHex port) ++ ")"

  show (CALL addr) = "CALL(" ++ (asHex addr) ++ ")"
  show (CALLCC cc addr) = "CALLCC(" ++ (show cc) ++ "," ++ (asHex addr) ++ ")"

  show RET = "RET"
  show (RETCC cc) = "RETCC(" ++ (show cc) ++ ")"

  show (PUSH rp) = "PUSH(" ++ (show rp) ++ ")"
  show (POP rp) = "POP(" ++ (show rp) ++ ")"

  show (RST nn) = "RST(" ++ (show nn) ++ ")"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

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

instance Show OperLD8 where
  show (Reg8Reg8 r r') = (show r) ++ ", " ++ (show r')
  show (Reg8Imm  r i ) = (show r) ++ ", " ++ (show i)
  show (HLIndLoad r) = (show r) ++ ", (HL)"
  show (IXIndLoad r disp) = (show r) ++ "(IX" ++ (showDisp disp) ++ ")"
  show (IYIndLoad r disp) = (show r) ++ "(IY" ++ (showDisp disp) ++ ")"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

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
  Imm16Indirect :: Z80addr
                -> OperLDA
  IReg          :: OperLDA
  RReg          :: OperLDA

instance Show OperLDA where
  show BCIndirect           = "[BC]"
  show DEIndirect           = "[DE]"
  show (Imm16Indirect addr) = "[" ++ (asHex addr) ++ "]"
  show IReg                 = "I"
  show RReg                 = "R"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | ALU operands
data OperALU where
  ALUimm        :: Z80word
                -> OperALU
  ALUreg8       :: Z80reg8
                -> OperALU
  ALUHLindirect :: OperALU
  ALUIXindirect :: Z80word
                -> OperALU
  ALUIYindirect :: Z80word
                -> OperALU

instance Show OperALU where
  show (ALUimm imm) = show imm
  show (ALUreg8 r8) = show r8
  show ALUHLindirect = "(HL)"
  show (ALUIXindirect disp) = "(IX " ++ (showDisp disp) ++ ")"
  show (ALUIYindirect disp) = "(IX " ++ (showDisp disp) ++ ")"

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

instance Show Z80condC where
  show NZ  = "NZ"
  show Z   = "Z"
  show NC  = "NC"
  show CY  = "C"
  show PO  = "PO"
  show PE  = "PE"
  show POS = "P"
  show MI  = "M"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Z80 8-bit registers
data Z80reg8 where
  A :: Z80reg8                                  -- Index 7
  B :: Z80reg8                                  -- Index 0
  C :: Z80reg8                                  -- Index 1
  D :: Z80reg8                                  -- Index 2
  E :: Z80reg8                                  -- Index 3
  H :: Z80reg8                                  -- Index 4
  L :: Z80reg8                                  -- Index 5
  HLindirect :: Z80reg8                         -- Index 6

instance Show Z80reg8 where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"
  show E = "E"
  show H = "H"
  show L = "L"
  show HLindirect = "(HL)"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data Z80reg16 where
  BC :: Z80reg16
  DE :: Z80reg16
  HL :: Z80reg16

instance Show Z80reg16 where
  show BC = "BC"
  show DE = "DE"
  show HL = "HL"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data RegPairSP where
  RPair16 :: Z80reg16
        -> RegPairSP
  SP :: RegPairSP

instance Show RegPairSP where
  show (RPair16 x) = show x
  show SP          = "SP"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data RegPairAF where
  RPair16' :: Z80reg16
           -> RegPairAF
  AF :: RegPairAF

instance Show RegPairAF where
  show (RPair16' x) = show x
  show AF           = "AF"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

showDisp :: Z80word
         -> String
showDisp disp
  | disp < 0  = (show disp)
  | otherwise = "+" ++ (show disp)