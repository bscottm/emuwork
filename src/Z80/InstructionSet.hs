{-# LANGUAGE GADTs #-}
-- | The Haskell representation of the Z80 instruction set
module Z80.InstructionSet
  ( Instruction(..)
  , Z80condC(..)
  , Z80reg8(..)
  , Z80reg16(..)
  , OperLD8(..)
  , AccumLoadStore(..)
  , OperALU(..)
  , OperExtendedALU(..)
  , OperIO(..)
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
  -- LD A, (BC)
  -- LD A, (DE)
  -- LD A, (nn)
  -- LD A, I
  -- LD A, R
  LDA :: AccumLoadStore
      -> Instruction
  -- LD (BC), A
  -- LD (DE), A
  -- LD (nn), A
  -- LD I, A
  -- LD R, A
  STA :: AccumLoadStore
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
  -- 16-bit indirect loads and stores, e.g. LD BC, (4000H) [load BC from the contents of 0x4000]
  LD16Indirect :: RegPairSP
               -> Z80addr
               -> Instruction
  ST16Indirect :: Z80addr
               -> RegPairSP
               -> Instruction
  -- Increment/decrement registers
  INC, DEC :: Z80reg8
           -> Instruction
  INC16, DEC16 :: RegPairSP
               -> Instruction
  --- ALU group: ADD, ADC, SUB, SBC, AND, XOR, OR and CP
  -- ADD HL, rp and ADC/SBC HL, rp
  SUB, AND, XOR, OR, CP :: OperALU
                        -> Instruction

  ADD :: OperExtendedALU
      -> Instruction
  ADC :: OperExtendedALU
      -> Instruction
  SBC :: OperExtendedALU
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
  IN, OUT :: OperIO
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

  -- 0xcb prefix instructions:
  -- RLC, RRC, RL, RR, SLA, SRA, SLL, SRL, BIT, RES, SET
  RLC, RRC, RL, RR, SLA, SRA, SLL, SRL :: Z80reg8
                                       -> Instruction
  BIT, RES, SET :: Z80word                      -- Bit within byte to set/reset/test
                -> Z80reg8
                -> Instruction

  -- 0xed prefix instructions:
  -- Negate accumulator
  NEG :: Instruction

  -- RETI, RETN: Return from interrupt, non-maskable interrupt
  RETI, RETN :: Instruction

  -- Interrupt mode
  IM :: Z80word
     -> Instruction

  -- Rotate right/left, decimal
  RRD, RLD :: Instruction

  -- Increment, Increment-Repeat instructions
  LDI, CPI, INI, OUTI, LDD, CPD, IND, OUTD, LDIR, CPIR, INIR, OTIR, LDDR, CPDR, INDR, OTDR :: Instruction
	
-- | Instruction -> String serialization
instance Show Instruction where
  show (Z80undef bytes) = "Z80undef " ++ (as0xHexS bytes)

  show (LD8 x) = "LD8(" ++ (show x) ++ ")"
  show (LDA x) = "LDA(" ++ (show x) ++ ")"
  show (STA x) = "STA(" ++ (show x) ++ ")"

  show (LD16 rp imm) = "LD16(" ++ (show rp) ++ "," ++ (as0xHexS imm) ++ ")"

  show (LDHL addr) = "LDHL(" ++ (as0xHexS addr) ++ ")"
  show (STHL addr) = "STHL(" ++ (as0xHexS addr) ++ ")"
  show (LD16Indirect rp addr) = "LD16Indirect(" ++ (show rp) ++ "," ++ (as0xHexS addr) ++ ")"
  show (ST16Indirect addr rp) = "LD16Indirect(" ++ (as0xHexS addr) ++ "," ++ (show rp) ++ ")"

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

  show (DJNZ addr) = "DJNZ(" ++ (as0xHexS addr) ++ ")"
  show (JR addr) = "JR(" ++ (as0xHexS addr) ++ ")"
  show (JRCC cc addr) = "JRCC(" ++ (show cc) ++ "," ++ (as0xHexS addr) ++ ")"

  show (JP addr) = "JP(" ++ (as0xHexS addr) ++ ")"
  show (JPCC cc addr) = "JPCC(" ++ (show cc) ++ "," ++ (show addr) ++ ")"

  show (ADD op) = "ADD(" ++ (show op) ++ ")"
  show (ADC op) = "ADC(" ++ (show op) ++ ")"
  show (SUB op) = "SUB(" ++ (show op) ++ ")"
  show (SBC op) = "SBC(" ++ (show op) ++ ")"
  show (AND op) = "AND(" ++ (show op) ++ ")"
  show (XOR op) = "XOR(" ++ (show op) ++ ")"
  show (OR op) = "OR(" ++ (show op) ++ ")"
  show (CP op) = "CP(" ++ (show op) ++ ")"

  show (INC reg) = "INC(" ++ (show reg) ++ ")"
  show (DEC reg) = "DEC(" ++ (show reg) ++ ")"
  show (INC16 rp) = "INC16(" ++ (show rp) ++ ")"
  show (DEC16 rp) = "DEC16(" ++ (show rp) ++ ")"

  show (IN port) = "IN(" ++ (show port) ++ ")"
  show (OUT port) = "OUT(" ++ (show port) ++ ")"

  show (CALL addr) = "CALL(" ++ (as0xHexS addr) ++ ")"
  show (CALLCC cc addr) = "CALLCC(" ++ (show cc) ++ "," ++ (as0xHexS addr) ++ ")"

  show RET = "RET"
  show (RETCC cc) = "RETCC(" ++ (show cc) ++ ")"

  show (PUSH rp) = "PUSH(" ++ (show rp) ++ ")"
  show (POP rp) = "POP(" ++ (show rp) ++ ")"

  show (RST nn) = "RST(" ++ (show nn) ++ ")"
  
  show (RLC r) = "RLC(" ++ (show r) ++ ")"
  show (RRC r) = "RRC(" ++ (show r) ++ ")"
  show (RL r) = "RL(" ++ (show r) ++ ")"
  show (RR r) = "RR(" ++ (show r) ++ ")"
  show (SLA r) = "SLA(" ++ (show r) ++ ")"
  show (SRA r) = "SRA(" ++ (show r) ++ ")"
  show (SLL r) = "SLL(" ++ (show r) ++ ")"
  show (SRL r) = "SRL(" ++ (show r) ++ ")"
  show (BIT bit r) = "BIT(" ++ (show bit) ++ "," ++ (show r) ++ ")"
  show (RES bit r) = "RES(" ++ (show bit) ++ "," ++ (show r) ++ ")"
  show (SET bit r) = "SET(" ++ (show bit) ++ "," ++ (show r) ++ ")"

  show NEG = "NEG"

  show RETI = "RETI"
  show RETN = "RETN"

  show (IM mode) = "IM(" ++ (show mode) ++ ")"

  show RLD = "RLD"
  show RRD = "RRD"

  show LDI = "LDI"
  show CPI = "CPI"
  show INI = "INI"
  show OUTI = "OUTI"
  show LDD = "LDD"
  show CPD = "CPD"
  show IND = "IND"
  show OUTD = "OUTD"
  show LDIR = "LDIR"
  show CPIR = "CPIR"
  show INIR = "INIR"
  show OTIR = "OTIR"
  show LDDR = "LDDR"
  show CPDR = "CPDR"
  show INDR = "INDR"
  show OTDR  = "OTDR"

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

-- | Accumulator load/store operands
data AccumLoadStore where
  BCIndirect    :: AccumLoadStore
  DEIndirect    :: AccumLoadStore
  Imm16Indirect :: Z80addr
                -> AccumLoadStore
  IReg          :: AccumLoadStore
  RReg          :: AccumLoadStore

instance Show AccumLoadStore where
  show BCIndirect            = "[BC]"
  show DEIndirect            = "[DE]"
  show (Imm16Indirect addr)  = "[" ++ (as0xHexS addr) ++ "]"
  show IReg                  = "I"
  show RReg                  = "R"

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

-- | ALU operations that can also extend to use HL and register pair
data OperExtendedALU where
  OrdinaryALU  :: OperALU
               -> OperExtendedALU
  HLRegPairALU :: RegPairSP
               -> OperExtendedALU

instance Show OperExtendedALU where
  show (OrdinaryALU op) = show op
  show (HLRegPairALU rp) = "HL," ++ (show rp)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data OperIO where
  PortImm :: Z80word
          -> OperIO
  CIndIO  :: Z80reg8
          -> OperIO
  CIndIO0 :: OperIO

instance Show OperIO where
  show (PortImm port) = as0xHexS port
  show (CIndIO reg8)  = show reg8 ++ ",(C)"
  show (CIndIO0)      = "(C)"

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