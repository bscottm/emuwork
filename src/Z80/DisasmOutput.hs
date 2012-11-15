{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Z80.DisasmOutput
  ( outputDisassembly
  ) where

import Data.Map
import Data.Char
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Vector.Unboxed as DVU

import Machine.Utils
import Machine.DisassemblerTypes
import Z80.InstructionSet
import Z80.Processor
import Z80.Disassembler

-- | The disassembly output function
outputDisassembly :: Z80Disassembly -> ByteString
outputDisassembly dis = doOutput (viewl dis) BC.empty
  where
    doOutput EmptyL acc = acc
    doOutput (left :< rest) acc = formatElem left (doOutput (viewl rest) acc)

    formatElem thing acc = case thing of
                             (DisasmInst addr bytes ins) -> BC.concat [ makeUpper $ asHex addr
                                                                      , ": "
                                                                      , formatBytes bytes
                                                                      , padTo 24 $ formatIns ins
                                                                      , "|"
                                                                      , formatChars bytes
                                                                      , "|"
                                                                      , (BC.cons '\n' acc)
                                                                      ]
                             (DisasmPseudo _) -> acc
      where
        formatBytes bytes = padTo 18 $ makeUpper $ DVU.foldl (\s x -> BC.concat [ s, asHex x, " " ]) BC.empty bytes
        formatChars bytes = padTo 6  $ DVU.foldl (\s x -> BC.append s (mkPrintable x)) BC.empty bytes
        formatIns ins = let (mnemonic, opers) = formatInstruction ins
                        in  BC.append (padTo 8 mnemonic) opers
        mkPrintable x = if x > 0x20 && x < 0x7f then (BC.singleton . chr . fromIntegral) x; else " "

formatInstruction :: Instruction
                  -> (ByteString, ByteString)

formatInstruction (Z80undef _) = zeroOperands "???"
formatInstruction (LD8 x) = oneOperand "LD" x
formatInstruction (LDA x) = oneOperand "LD" x
formatInstruction (STA x) = oneOperand "LD" x
formatInstruction (LD16 r imm) = twoOperands "LD" r imm
formatInstruction (STHL addr) = ("LD", BC.append "(" (BC.append (formatOperand addr) "), HL"))
formatInstruction (LDHL addr) = ("LD", BC.append "HL, (" (BC.append (formatOperand addr) ")"))
formatInstruction (INC r) = oneOperand "INC" r
formatInstruction (DEC r) = oneOperand "DEC" r
formatInstruction (INC16 r) = oneOperand "INC" r
formatInstruction (DEC16 r) = oneOperand "DEC" r
formatInstruction (ADD r) = oneOperand "ADD" r
formatInstruction (ADC r) = oneOperand "ADC" r
formatInstruction (SUB r) = oneOperand "SUB" r
formatInstruction (SBC r) = oneOperand "SBC" r
formatInstruction (AND r) = oneOperand "AND" r
formatInstruction (XOR r) = oneOperand "XOR" r
formatInstruction (OR r) = oneOperand "OR" r
formatInstruction (CP r) = oneOperand "CP" r
formatInstruction (ADDHL r) = ("ADD", BC.append "HL, " $ formatOperand r)
formatInstruction HALT = zeroOperands "HALT"
formatInstruction NOP = zeroOperands "NOP"
formatInstruction EXAFAF' = ("EX", "AF, AF'")
formatInstruction EXSPHL = ("EX", "(SP), HL")
formatInstruction EXDEHL = ("EX", "DE, HL")
formatInstruction DI = zeroOperands "DI"
formatInstruction EI = zeroOperands "EI"
formatInstruction EXX = zeroOperands "EXX"
formatInstruction JPHL = ("JP", "HL")
formatInstruction LDSPHL = ("LD", "SP, HL")
formatInstruction RLCA = zeroOperands "RLCA"
formatInstruction RRCA = zeroOperands "RRCA"
formatInstruction RLA = zeroOperands "RLA"
formatInstruction RRA = zeroOperands "RRA"
formatInstruction DAA = zeroOperands "DAA"
formatInstruction CPL = zeroOperands "CPL"
formatInstruction SCF = zeroOperands "SCF"
formatInstruction CCF = zeroOperands "CCF"
formatInstruction (DJNZ addr) = oneOperand "DJNZ" addr
formatInstruction (JR addr) = oneOperand "JR" addr
formatInstruction (JRCC cc addr) = twoOperands "JR" cc addr
formatInstruction (JP addr) = oneOperand "JP" addr
formatInstruction (JPCC cc addr) = twoOperands "JP" cc addr
formatInstruction (IN port) = oneOperand "IN" port
formatInstruction (OUT port) = oneOperand "OUT" port
formatInstruction (CALL addr) = oneOperand "CALL" addr
formatInstruction (CALLCC cc addr) = twoOperands "CALL" cc addr
formatInstruction RET = zeroOperands "RET"
formatInstruction (RETCC cc) = oneOperand "RET" cc
formatInstruction (PUSH r) = oneOperand "PUSH" r
formatInstruction (POP r) = oneOperand "POP" r
formatInstruction (RST rst) = oneOperand "RST" rst

formatInstruction (RLC r) = oneOperand "RLC" r
formatInstruction (RRC r) = oneOperand "RRC" r
formatInstruction (RL r) = oneOperand "RL" r
formatInstruction (RR r) = oneOperand "RR" r
formatInstruction (SLA r) = oneOperand "SLA" r
formatInstruction (SRA r) = oneOperand "SRA" r
formatInstruction (SLL r) = oneOperand "SLL" r
formatInstruction (SRL r) = oneOperand "SRL" r
formatInstruction (BIT bit r) = twoOperands "BIT" bit r
formatInstruction (RES bit r) = twoOperands "RES" bit r
formatInstruction (SET bit r) = twoOperands "SET" bit r
formatInstruction LDI = zeroOperands "LDI"
formatInstruction CPI = zeroOperands "CPI"
formatInstruction INI = zeroOperands "INI"
formatInstruction OUTI = zeroOperands "OUTI"
formatInstruction LDD = zeroOperands "LDD"
formatInstruction CPD = zeroOperands "CPD"
formatInstruction IND = zeroOperands "IND"
formatInstruction OUTD = zeroOperands "OUTD"
formatInstruction LDIR = zeroOperands "LDIR"
formatInstruction CPIR = zeroOperands "CPIR"
formatInstruction INIR = zeroOperands "INIR"
formatInstruction OTIR = zeroOperands "OTIR"
formatInstruction LDDR = zeroOperands "LDDR"
formatInstruction CPDR = zeroOperands "CPDR"
formatInstruction INDR = zeroOperands "INDR"
formatInstruction OTDR = zeroOperands "OTDR"

-- formatInstruction _ = zeroOperands "--!!"

-- | Disassembly output with an instruction having no operands
zeroOperands :: ByteString 
             -> (ByteString, ByteString)
zeroOperands mne = (mne, BC.empty)

-- | Disassembly output with an instruction having one operand
oneOperand :: forall operand. (DisOperandFormat operand) =>
              ByteString
           -> operand
           -> (ByteString, ByteString)
oneOperand mne op = (mne, formatOperand op)

-- | Disassembly output with a two operand instruction
twoOperands :: forall operand1 operand2. (DisOperandFormat operand1, DisOperandFormat operand2) =>
               ByteString
            -> operand1
            -> operand2
            -> (ByteString, ByteString)
twoOperands mne op1 op2 = (mne, BC.concat [ formatOperand op1
                                          , ", "
                                          , formatOperand op2
                                          ]
                          )

class DisOperandFormat x where
  formatOperand :: x -> ByteString

instance DisOperandFormat Z80word where
  formatOperand x = BC.append (makeUpper . asHex $ x) "H"

instance DisOperandFormat Z80addr where
  formatOperand x = BC.append (makeUpper . asHex $ x) "H"

instance DisOperandFormat OperLD8 where
  formatOperand (Reg8Reg8 r r') = BC.append (formatOperand r) (BC.append ", " (formatOperand r'))
  formatOperand (Reg8Imm r imm) = BC.append (formatOperand r) (BC.append ", " (formatOperand imm))
  formatOperand (HLIndLoad r)   = BC.append (formatOperand r) ", (HL)"
  formatOperand (IXIndLoad r disp) = BC.append (formatOperand r) (BC.append ", (IX" (BC.append (showDisp disp) ")"))
  formatOperand (IYIndLoad r disp) = BC.append (formatOperand r) (BC.append ", (IY" (BC.append (showDisp disp) ")"))

instance DisOperandFormat OperLDA where
  formatOperand BCIndirect = "A, (BC)"
  formatOperand DEIndirect = "A, (DE)"
  formatOperand (Imm16Indirect addr) = BC.append "A, (" (BC.append (formatOperand addr) ")")
  formatOperand IReg = "A, I"
  formatOperand RReg = "A, R"

instance DisOperandFormat OperSTA where
  formatOperand BCIndirect' = "(BC), A"
  formatOperand DEIndirect' = "(DE), A"
  formatOperand (Imm16Indirect' addr) = BC.append "(" (BC.append (formatOperand addr) "), A")
  formatOperand IReg' = "I, A"
  formatOperand RReg' = "R, A"

instance DisOperandFormat OperALU where
  formatOperand (ALUimm imm) = formatOperand imm
  formatOperand (ALUreg8 r) = formatOperand r
  formatOperand (ALUHLindirect) = "(HL)"
  formatOperand (ALUIXindirect disp) = BC.append ", (IX" (BC.append (showDisp disp) ")")
  formatOperand (ALUIYindirect disp) = BC.append ", (IY" (BC.append (showDisp disp) ")")

instance DisOperandFormat RegPairSP where
  formatOperand (RPair16 r) = formatOperand r
  formatOperand SP = "SP"

instance DisOperandFormat Z80reg8 where
  formatOperand A = "A"
  formatOperand B = "B"
  formatOperand C = "C"
  formatOperand D = "D"
  formatOperand E = "E"
  formatOperand H = "H"
  formatOperand L = "L"
  formatOperand HLindirect = "(HL)"

instance DisOperandFormat Z80reg16 where
  formatOperand BC = "BC"
  formatOperand DE = "DE"
  formatOperand HL = "HL"

instance DisOperandFormat Z80condC where
  formatOperand NZ  = "NZ"
  formatOperand Z = "Z"
  formatOperand NC = "NC"
  formatOperand CY = "C"
  formatOperand PO = "PO"
  formatOperand PE = "PE"
  formatOperand POS = "P"
  formatOperand MI = "M"

instance DisOperandFormat RegPairAF where
  formatOperand (RPair16' r) = formatOperand r
  formatOperand AF = "AF"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

showDisp :: Z80word
         -> ByteString
showDisp disp
  | disp < 0  = BC.pack . show $ disp
  | otherwise = BC.append "+" (BC.pack . show $ disp)