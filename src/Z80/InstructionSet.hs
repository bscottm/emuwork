-- | The Haskell representation of the Z80 instruction set
module Z80.InstructionSet
  ( -- * Types
    Z80instruction(..)
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
  ) where

import Control.Lens

import Machine
import Z80.Processor

-- | The Z80 instruction set
data Z80instruction where
  -- Undefined/invalid instruction
  Z80undef :: [Z80word]
           -> Z80instruction
  -- 8-bit load group:
  -- LD r, r'
  -- LD r, n
  -- LD r, (HL)
  -- LD r, (IX + d)
  -- LD r, (IY + d)
  LD8 :: OperLD8
      -> Z80instruction
  -- LD A, (BC)
  -- LD A, (DE)
  -- LD A, (nn)
  -- LD A, I
  -- LD A, R
  LDA :: AccumLoadStore
      -> Z80instruction
  -- LD (BC), A
  -- LD (DE), A
  -- LD (nn), A
  -- LD I, A
  -- LD R, A
  STA :: AccumLoadStore
      -> Z80instruction
  -- LD rp, nn
  LD16 :: RegPairSP
       -> SymAbsAddr Z80addr
       -> Z80instruction
  -- LD (nn), HL
  -- LD HL, (nn)
  STHL :: SymAbsAddr Z80addr
       -> Z80instruction
  LDHL :: SymAbsAddr Z80addr
       -> Z80instruction
  -- 16-bit indirect loads and stores, e.g. LD BC, (4000H) [load BC from the contents of 0x4000]
  LD16Indirect :: RegPairSP
               -> Z80addr
               -> Z80instruction
  ST16Indirect :: Z80addr
               -> RegPairSP
               -> Z80instruction
  -- Increment/decrement registers
  INC, DEC :: Z80reg8
           -> Z80instruction
  INC16, DEC16 :: RegPairSP
               -> Z80instruction
  --- ALU group: ADD, ADC, SUB, SBC, AND, XOR, OR and CP
  -- ADD HL, rp and ADC/SBC HL, rp
  SUB, AND, XOR, OR, CP :: OperALU
                        -> Z80instruction

  ADD :: OperExtendedALU
      -> Z80instruction
  ADC :: OperExtendedALU
      -> Z80instruction
  SBC :: OperExtendedALU
      -> Z80instruction

  -- HALT; NOP; Exchanges; DI; EI; JP HL; LD SP, HL
  HALT, NOP, DI, EI, JPHL, LDSPHL :: Z80instruction
  -- Exchanges:
  EXC :: Z80ExchangeOper
      -> Z80instruction
  -- Accumulator ops: RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
  RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF :: Z80instruction
  -- Relative jumps: DJNZ and JR. Note: Even though these are relative jumps, the address is stored
  -- since it's easy to recompute the displacement.
  DJNZ :: SymAbsAddr Z80addr
       -> Z80instruction
  JR   :: SymAbsAddr Z80addr
       -> Z80instruction
  JRCC :: Z80condC
       -> SymAbsAddr Z80addr
       -> Z80instruction
  -- Jumps
  JP :: SymAbsAddr Z80addr
     -> Z80instruction
  JPCC :: Z80condC
       -> SymAbsAddr Z80addr
       -> Z80instruction
  -- I/O instructions
  IN, OUT :: OperIO
          -> Z80instruction
  -- Subroutine call
  CALL :: SymAbsAddr Z80addr
       -> Z80instruction
  CALLCC :: Z80condC
         -> SymAbsAddr Z80addr
         -> Z80instruction
  -- Return
  RET :: Z80instruction
  RETCC :: Z80condC
        -> Z80instruction
  -- Push, pop
  PUSH, POP :: RegPairAF
            -> Z80instruction
  -- Restart
  RST :: Z80word
      -> Z80instruction

  -- 0xcb prefix instructions:
  -- RLC, RRC, RL, RR, SLA, SRA, SLL, SRL, BIT, RES, SET
  RLC, RRC, RL, RR, SLA, SRA, SLL, SRL :: Z80reg8
                                       -> Z80instruction
  BIT, RES, SET :: Z80word                      -- Bit within byte to set/reset/test
                -> Z80reg8
                -> Z80instruction

  -- 0xed prefix instructions:
  -- Negate accumulator
  NEG :: Z80instruction

  -- RETI, RETN: Return from interrupt, non-maskable interrupt
  RETI, RETN :: Z80instruction

  -- Interrupt mode
  IM :: Z80word
     -> Z80instruction

  -- Rotate right/left, decimal
  RRD, RLD :: Z80instruction

  -- Increment, Increment-Repeat instructions
  LDI, CPI, INI, OUTI, LDD, CPD, IND, OUTD, LDIR, CPIR, INIR, OTIR, LDDR, CPDR, INDR, OTDR :: Z80instruction
        
-- | Z80instruction -> String serialization
instance Show Z80instruction where
  show (Z80undef bytes) = "Z80undef " ++ (as0xHexS bytes)

  show (LD8 x) = "LD8(" ++ (show x) ++ ")"
  show (LDA x) = "LDA(" ++ (show x) ++ ")"
  show (STA x) = "STA(" ++ (show x) ++ ")"

  show (LD16 rp imm) = "LD16(" ++ (show rp) ++ "," ++ (show imm) ++ ")"

  show (LDHL addr) = "LDHL(" ++ (show addr) ++ ")"
  show (STHL addr) = "STHL(" ++ (show addr) ++ ")"
  show (LD16Indirect rp addr) = "LD16Indirect(" ++ (show rp) ++ "," ++ (as0xHexS addr) ++ ")"
  show (ST16Indirect addr rp) = "LD16Indirect(" ++ (as0xHexS addr) ++ "," ++ (show rp) ++ ")"

  show HALT = "HALT"
  show NOP = "NOP"
  show (EXC exc) = "EXC(" ++ (show exc) ++ ")"
  show DI = "DI"
  show EI = "EI"
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

  show (DJNZ addr) = "DJNZ(" ++ (show addr) ++ ")"
  show (JR addr) = "JR(" ++ (show addr) ++ ")"
  show (JRCC cc addr) = "JRCC(" ++ (show cc) ++ "," ++ (show addr) ++ ")"

  show (JP addr) = "JP(" ++ (show addr) ++ ")"
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

  show (CALL addr) = "CALL(" ++ (show addr) ++ ")"
  show (CALLCC cc addr) = "CALLCC(" ++ (show cc) ++ "," ++ (show addr) ++ ")"

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
  show (BIT bitno r) = "BIT(" ++ (show bitno) ++ "," ++ (show r) ++ ")"
  show (RES bitno r) = "RES(" ++ (show bitno) ++ "," ++ (show r) ++ ")"
  show (SET bitno r) = "SET(" ++ (show bitno) ++ "," ++ (show r) ++ ")"

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
             -> Z80disp
             -> OperLD8
  IYIndLoad :: Z80reg8
             -> Z80disp
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
  Imm16Indirect :: SymAbsAddr Z80addr
                -> AccumLoadStore
  IReg          :: AccumLoadStore
  RReg          :: AccumLoadStore

instance Show AccumLoadStore where
  show BCIndirect            = "[BC]"
  show DEIndirect            = "[DE]"
  show (Imm16Indirect addr)  = "[" ++ (show addr) ++ "]"
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

instance Show OperALU where
  show (ALUimm imm) = show imm
  show (ALUreg8 r8) = show r8
  show ALUHLindirect = "(HL)"

-- | ALU operations that can also extend to use HL and register pair
data OperExtendedALU where
  ALU8  :: OperALU
        -> OperExtendedALU
  ALU16 :: RegPairSP
        -> OperExtendedALU

instance Show OperExtendedALU where
  show (ALU8 op)  = show op
  show (ALU16 rp) = "HL," ++ (show rp)

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

instance Show Z80reg8 where
  show A                  = "A"
  show B                  = "B"
  show C                  = "C"
  show D                  = "D"
  show E                  = "E"
  show H                  = "H"
  show L                  = "L"
  show HLindirect         = "(HL)"
  show (IXindirect disp)  = "(IX" ++ (show disp) ++ ")"
  show (IYindirect disp)  = "(IY" ++ (show disp) ++ ")"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data Z80reg16 where
  BC :: Z80reg16
  DE :: Z80reg16
  HL :: Z80reg16
  IX :: Z80reg16
  IY :: Z80reg16

instance Show Z80reg16 where
  show BC = "BC"
  show DE = "DE"
  show HL = "HL"
  show IX = "IX"
  show IY = "IY"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes SP (instead of AF)
data RegPairSP where
  RPair16 :: Z80reg16
          -> RegPairSP
  SP      :: RegPairSP

instance Show RegPairSP where
  show (RPair16 x) = show x
  show SP          = "SP"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Register pair that includes AF (instead of SP)
data RegPairAF where
  RPair16' :: Z80reg16
           -> RegPairAF
  AF       :: RegPairAF

instance Show RegPairAF where
  show (RPair16' x) = show x
  show AF           = "AF"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Exchange instruction arguments
data Z80ExchangeOper where
  AFAF'  :: Z80ExchangeOper     -- AF with AF'
  DEHL   :: Z80ExchangeOper     -- DE with HL
  SPHL   :: Z80ExchangeOper     -- SP with HL
  Primes :: Z80ExchangeOper     -- EXX (regular <-> primes)

instance Show Z80ExchangeOper where
  show AFAF'  = "AFAF'"
  show DEHL   = "DEHL"
  show SPHL   = "SPHL"
  show Primes = "Primes"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Show a one byte displacement ('Z80word', aka 'Word', is a signed quantity.)
showDisp :: Z80disp
         -> String
showDisp disp
  | disp < 0  = (show disp)
  | otherwise = "+" ++ (show disp)

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
