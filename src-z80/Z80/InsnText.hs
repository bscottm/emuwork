{- | Z80 instructions to text representation.


-}
module Z80.InsnText 
  ( instructionToText
  , upperHex
  , oldStyleHex
  , Z80operandFormat(..)
  ) where

import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as T

import           Machine            (AbstractAddr (..), ShowHex (asHex), makeUpper)

import           Z80.InstructionSet
import           Z80.Processor      (Z80addr, Z80byte)

-- | Convert a Z80 instruction to its textual representation. Returns
-- a tuple '(mnemonic, operands)'.
instructionToText
  :: Z80instruction
  -> (T.Text, T.Text)
instructionToText (Z80undef bytes)    = ("???", formatOperand bytes)
instructionToText (LD opnd)           = ("LD", formatOperand opnd)
instructionToText (LD16 opnd)         = ("LD", formatOperand opnd)
instructionToText (INC opnd)          = ("INC", formatOperand opnd)
instructionToText (DEC opnd)          = ("DEC", formatOperand opnd)
instructionToText (ADD opnd)          = ("ADD", T.append "A, " (formatOperand opnd))
instructionToText (ADD16 opnd)        = ("ADD", formatOperand opnd)
instructionToText (ADC opnd)          = ("ADC", T.append "A, " (formatOperand opnd))
instructionToText (ADC16 opnd)        = ("ADC", formatOperand opnd)
instructionToText (SUB opnd)          = ("SUB", formatOperand opnd)
instructionToText (SBC opnd)          = ("SBC", T.append "A, " (formatOperand opnd))
instructionToText (SBC16 opnd)        = ("SBC", formatOperand opnd)
instructionToText (AND opnd)          = ("AND", formatOperand opnd)
instructionToText (XOR opnd)          = ("XOR", formatOperand opnd)
instructionToText (OR opnd)           = ("OR", formatOperand opnd)
instructionToText (CP opnd)           = ("CP", formatOperand opnd)
instructionToText HALT                = ("HALT", T.empty)
instructionToText NOP                 = ("NOP", T.empty)
instructionToText (EXC Primes)        = ("EXX", T.empty)
instructionToText (EXC opnd)          = ("EX", formatOperand opnd)
instructionToText DI                  = ("DI", T.empty)
instructionToText EI                  = ("EI", T.empty)
instructionToText JPHL                = ("JP", "(HL)")
instructionToText JPIX                = ("JP", "(IX)")
instructionToText JPIY                = ("JP", "(IY)")
instructionToText LDSPHL              = ("LD", "SP, HL")
instructionToText LDSPIX              = ("LD", "SP, IX")
instructionToText LDSPIY              = ("LD", "SP, IY")
instructionToText RLCA                = ("RLCA", T.empty)
instructionToText RRCA                = ("RRCA", T.empty)
instructionToText RLA                 = ("RLA", T.empty)
instructionToText RRA                 = ("RRA", T.empty)
instructionToText DAA                 = ("DAA", T.empty)
instructionToText CPL                 = ("CPL", T.empty)
instructionToText SCF                 = ("SCF", T.empty)
instructionToText CCF                 = ("CCF", T.empty)
instructionToText (DJNZ opnd)         = ("DJNZ", formatOperand opnd)
instructionToText (JR opnd)           = ("JR", formatOperand opnd)
instructionToText (JRCC cc opnd)      = ("JR", T.intercalate ", " [formatOperand cc, formatOperand opnd])
instructionToText (JP opnd)           = ("JP", formatOperand opnd)
instructionToText (JPCC cc opnd)      = ("JP", T.intercalate ", " [formatOperand cc, formatOperand opnd])
instructionToText (IN (PortImm imm))  = ("IN", T.concat ["A, ", formatOperand imm])
instructionToText (IN (CIndIO reg8))  = ("IN", T.append (formatOperand reg8) ", (C)")
instructionToText (IN CIndIO0)        = ("IN", "(C)")
instructionToText (OUT (PortImm imm)) = ("OUT", T.concat [formatOperand imm, ", A"])
instructionToText (OUT (CIndIO reg8)) = ("OUT", T.append "(C), " (formatOperand reg8))
instructionToText (OUT CIndIO0)       = ("OUT", "(C), 0")
instructionToText (CALL opnd)         = ("CALL", formatOperand opnd)
instructionToText (CALLCC cc opnd)    = ("CALL", T.intercalate ", " [formatOperand cc, formatOperand opnd])
instructionToText RET                 = ("RET", T.empty)
instructionToText (RETCC opnd)        = ("RET", formatOperand opnd)
instructionToText (PUSH opnd)         = ("PUSH", formatOperand opnd)
instructionToText (POP _)             = ("POP", T.empty)
instructionToText (RST opnd)          = ("RST", asHex opnd)
instructionToText (RLC opnd)          = ("RLC", formatOperand opnd)
instructionToText (RRC opnd)          = ("RRC", formatOperand opnd)
instructionToText (RL opnd)           = ("RL", formatOperand opnd)
instructionToText (RR opnd)           = ("RR", formatOperand opnd)
instructionToText (SLA opnd)          = ("SLA", formatOperand opnd)
instructionToText (SRA opnd)          = ("SRA", formatOperand opnd)
instructionToText (SLL opnd)          = ("SLL", formatOperand opnd)
instructionToText (SRL opnd)          = ("SRL", formatOperand opnd)
instructionToText (BIT bit reg)       = ("BIT", T.intercalate ", " [ formatOperand bit, formatOperand reg])
instructionToText (RES bit reg)       = ("RES", T.intercalate ", " [ formatOperand bit, formatOperand reg])
instructionToText (SET bit reg)       = ("SET", T.intercalate ", " [ formatOperand bit, formatOperand reg])
instructionToText NEG                 = ("NEG", T.empty)
instructionToText RETI                = ("RETI", T.empty)
instructionToText RETN                = ("RETN", T.empty)
instructionToText (IM opnd)           = ("IM", formatOperand opnd)
instructionToText RLD                 = ("RLD", T.empty)
instructionToText RRD                 = ("RRD", T.empty)
instructionToText LDI                 = ("LDI", T.empty)
instructionToText CPI                 = ("CPI", T.empty)
instructionToText INI                 = ("INI", T.empty)
instructionToText OUTI                = ("OUTI", T.empty)
instructionToText LDD                 = ("LDD", T.empty)
instructionToText CPD                 = ("CPD", T.empty)
instructionToText IND                 = ("IND", T.empty)
instructionToText OUTD                = ("OUTD", T.empty)
instructionToText LDIR                = ("LDIR", T.empty)
instructionToText CPIR                = ("CPIR", T.empty)
instructionToText INIR                = ("INIR", T.empty)
instructionToText OTIR                = ("OTIR", T.empty)
instructionToText LDDR                = ("LDDR", T.empty)
instructionToText CPDR                = ("CPDR", T.empty)
instructionToText INDR                = ("INDR", T.empty)
instructionToText OTDR                = ("OTDR", T.empty)
-- The undocumented CB prefixed instructions
instructionToText (RLCidx reg1 reg2)  = ("RLC", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (RRCidx reg1 reg2)  = ("RRC", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (RLidx reg1 reg2)   = ("RL", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (RRidx reg1 reg2)   = ("RR", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (SLAidx reg1 reg2)  = ("SLA", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (SRAidx reg1 reg2)  = ("SRA", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (SLLidx reg1 reg2)  = ("SLL", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (SRLidx reg1 reg2)  = ("SRL", T.intercalate ", " (formatOperand <$> [reg1, reg2]))
instructionToText (RESidx bit reg1 reg2) = ("RES", T.concat [ formatOperand bit, ", ", formatOperand reg1, ", ", formatOperand reg2])
instructionToText (SETidx bit reg1 reg2) = ("SET", T.concat [ formatOperand bit, ", ", formatOperand reg1, ", ", formatOperand reg2])

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassembler operand format function class. Make life easy on ourselves when
-- formatting assembler operands.
class Z80operandFormat x where
  -- | Convert an operand into its appropriate representation
  formatOperand :: x -> T.Text

instance (Z80operandFormat a)
         => Z80operandFormat [a] where
  formatOperand = T.concat . map formatOperand

instance Z80operandFormat Z80byte where
  formatOperand = oldStyleHex

instance Z80operandFormat Z80addr where
  formatOperand = oldStyleHex

instance Z80operandFormat (Z80operand Z80OpndUndef) where
  formatOperand (UndefInsn bytes) = T.intercalate ", " (formatOperand <$> bytes)

instance Z80operandFormat (Z80operand Z80OpndLoad) where
  formatOperand (Reg8Reg8 r r')           = T.intercalate ", " (formatOperand <$> [r, r'])
  formatOperand (Reg8Imm r imm)           = T.intercalate ", " [formatOperand r, formatOperand imm]
  formatOperand FromBCindirect            = "A, (BC)"
  formatOperand FromDEindirect            = "A, (DE)"
  formatOperand ToBCindirect              = "(BC), A"
  formatOperand ToDEindirect              = "(DE), A"
  formatOperand (AccFromMem addr)         = T.concat [ "A, (", formatOperand addr , ")" ]
  formatOperand (AccToMem addr)           = T.concat [ "(", formatOperand addr, "), A" ]
  formatOperand (Reg16Imm rp imm)         = T.intercalate ", " [formatOperand rp, formatOperand imm]
  formatOperand FromItoA                   = "A, I"
  formatOperand FromRtoA                   = "A, R"
  formatOperand FromAtoI                   = "I, A"
  formatOperand FromAtoR                   = "R, A"

instance Z80operandFormat (Z80operand Z80OpndLoad16) where
  formatOperand (ToReg16 rp addr)         = T.concat [ formatOperand rp , ", (" , formatOperand addr , ")" ]
  formatOperand (FromReg16 rp addr)       = T.concat [ "(" , formatOperand addr , "), " , formatOperand rp ]

instance Z80operandFormat (Z80operand Z80OpndALU) where
  formatOperand (ALUimm imm)  = formatOperand imm
  formatOperand (ALUreg8 r)   = formatOperand r

instance Z80operandFormat (Z80operand Z80OpndInc) where
  formatOperand (IncDecReg8 reg) = formatOperand reg
  formatOperand (IncDecReg16 reg) = formatOperand reg

instance Z80operandFormat (Z80operand Z80OpndALU16) where
  formatOperand (DestHL reg16) = T.append "HL, " $ formatOperand reg16
  formatOperand (DestIX reg16) = T.append "IX, " $ formatOperand reg16
  formatOperand (DestIY reg16) = T.append "IY, " $ formatOperand reg16

instance Z80operandFormat RegPairSP where
  formatOperand (RPair16 r) = formatOperand r
  formatOperand SP          = "SP"

instance Z80operandFormat Z80reg8 where
  formatOperand A = "A"
  formatOperand B = "B"
  formatOperand C = "C"
  formatOperand D = "D"
  formatOperand E = "E"
  formatOperand H = "H"
  formatOperand L = "L"
  formatOperand HLindirect = "(HL)"
  formatOperand (IXindirect disp) = showDisp "IX" disp
  formatOperand (IYindirect disp) = showDisp "IY" disp
  formatOperand IXh = "IXh"
  formatOperand IXl = "IXl"
  formatOperand IYh = "IYh"
  formatOperand IYl = "IYl"

instance Z80operandFormat Z80reg16 where
  formatOperand BC = "BC"
  formatOperand DE = "DE"
  formatOperand HL = "HL"
  formatOperand IX = "IX"
  formatOperand IY = "IY"

instance Z80operandFormat Z80condC where
  formatOperand NZ    = "NZ"
  formatOperand Z     = "Z"
  formatOperand NC    = "NC"
  formatOperand CY    = "C"
  formatOperand PO    = "PO"
  formatOperand PE    = "PE"
  formatOperand POS   = "P"
  formatOperand MI    = "M"

instance Z80operandFormat RegPairAF where
  formatOperand (AFPair16 r) = formatOperand r
  formatOperand AF           = "AF"

instance (Z80operandFormat addrType) => Z80operandFormat (AbstractAddr addrType) where
  formatOperand (AbstractAddr addr label)  = fromMaybe (formatOperand addr) label

instance Z80operandFormat Z80ExchangeOper where
  formatOperand AFAF'  = "AF, AF'"
  formatOperand DEHL   = "DE, HL"
  formatOperand SPHL   = "(SP), HL"
  formatOperand SPIX   = "(SP), IX"
  formatOperand SPIY   = "(SP), IY"
  formatOperand Primes = T.empty

instance Z80operandFormat (Z80operand Z80OpndIO) where
  formatOperand _ =  T.empty

-- | Output an 'upperHex' address with the appended 'H'
oldStyleHex :: ShowHex operand =>
               operand
            -> T.Text
oldStyleHex x = T.snoc (upperHex x) 'H'

-- | Output a formatted address in uppercase hex
upperHex :: ShowHex operand =>
            operand
         -> T.Text
upperHex = makeUpper . asHex

showDisp :: (Integral dispT, Show dispT)
         => T.Text
         -> dispT
         -> T.Text
showDisp ixReg disp = T.concat ["(", ixReg, theDisp, ")"]
  where
    fmtDisp = T.pack . show $ disp
    theDisp
      | disp < 0  = fmtDisp
      | otherwise = T.append "+" fmtDisp
