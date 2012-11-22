{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Z80.DisasmOutput
  ( outputDisassembly
  ) where

-- import Debug.Trace

import Data.Int
import Data.Char
import Data.Label
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Map as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as DVU

import Machine.Utils
import Machine.DisassemblerTypes
import Z80.InstructionSet
import Z80.Processor
import Z80.Disassembler

-- | Emit Template Haskell hair for lenses
mkLabel ''Disassembly

-- | The disassembly output function
outputDisassembly :: Z80Disassembly -> ByteString
outputDisassembly dis = doOutput (viewl (get disasmSeq dis)) BC.empty
  where
    doOutput EmptyL acc = acc
    doOutput (left :< rest) acc = formatElem left (doOutput (viewl rest) acc)

    formatElem thing acc = case thing of
                             (DisasmInst addr bytes ins) -> BC.concat [ formatLinePrefix bytes addr dis
                                                                      , padTo lenInstruction $ formatIns ins
                                                                      , "\n"
                                                                      , acc
                                                                      ]
                             (DisasmPseudo pseudo)       -> formatPseudo pseudo dis acc
      where
        formatIns ins = let (mnemonic, opers) = formatInstruction dis ins
                        in  BC.append (padTo lenMnemonic mnemonic) opers

-- | Output a formatted address in uppercase hex
upperHex :: forall operand. ShowHex operand => 
            operand
         -> ByteString
upperHex = (makeUpper . asHex)

-- | Output an 'upperHex' address with the appended 'H'
oldStyleHex :: forall operand. ShowHex operand =>
               operand
            -> ByteString
oldStyleHex x = BC.snoc (upperHex x) 'H'

-- | Format the beginning of the line (address, bytes, label, etc.)
formatLinePrefix :: Vector Z80word
                 -> Z80addr
                 -> Z80Disassembly
                 -> ByteString
formatLinePrefix bytes addr dstate =
  let label               = case Map.lookup addr (get symbolTab dstate) of
                              Nothing  -> ""
                              Just lab -> BC.snoc lab ':'
      bytesToChars vec    = padTo lenAsChars  $ DVU.foldl (\s x -> BC.append s (mkPrintable x)) BC.empty vec
      mkPrintable x       = if x > 0x20 && x < 0x7f then (BC.singleton . chr . fromIntegral) x; else " "
  in  if BC.length label < (lenSymLabel - 2) then
        BC.concat [ upperHex addr
                  , ": "
                  , formatBytes bytes
                  , "|"
                  , bytesToChars bytes
                  , "| "
                  , padTo lenSymLabel label
                  ]
      else BC.concat [ upperHex addr
                     , ": "
                     , padTo (lenInsBytes + lenAsChars + 3) ""
                     , label
                     , "\n"
                     , upperHex addr
                     , ": "
                     , formatBytes bytes
                     , "|"
                     , bytesToChars bytes
                     , "| "
                     , padTo lenSymLabel ""
                     ]

-- | Format a series of bytes
formatBytes :: Vector Z80word
            -> ByteString
formatBytes bytes = padTo lenInsBytes $ BC.intercalate " " [ upperHex x | x <- DVU.toList bytes ]

-- Lengths of various output columns:

lenInsBytes :: Int64
lenInsBytes = (lenAsChars * 3)          -- ^ 8 bytes max to dump, 3 spaces per byte

lenAsChars :: Int64
lenAsChars = 8                          -- ^ 8 characters, as the bytes are dumped as characters

lenSymLabel :: Int64
lenSymLabel = BC.length("XXXXXXXXXXXX") + 2     -- ^ Label colum is 8 characters plus ": "

lenInstruction :: Int64
lenInstruction = 24

lenMnemonic :: Int64
lenMnemonic = 8

formatInstruction :: Z80Disassembly
                  -> Instruction
                  -> (ByteString, ByteString)

formatInstruction _dstate (Z80undef _) = zeroOperands "???"
formatInstruction _dstate (LD8 x) = oneOperand "LD" x
formatInstruction _dstate (LDA x) = ("LD", (accumLoadStore x False))
formatInstruction _dstate (STA x) = ("LD", (accumLoadStore x True))
formatInstruction _dstate (LD16 r imm) = twoOperands "LD" r imm
formatInstruction _dstate (STHL addr) = ("LD", BC.append "(" (BC.append (formatOperand addr) "), HL"))
formatInstruction _dstate (LDHL addr) = ("LD", BC.append "HL, (" (BC.append (formatOperand addr) ")"))
formatInstruction _dstate (LD16Indirect rp addr) = ("LD", indirect16LoadStore rp addr False)
formatInstruction _dstate (ST16Indirect addr rp) = ("LD", indirect16LoadStore rp addr True)
formatInstruction _dstate (INC r) = oneOperand "INC" r
formatInstruction _dstate (DEC r) = oneOperand "DEC" r
formatInstruction _dstate (INC16 r) = oneOperand "INC" r
formatInstruction _dstate (DEC16 r) = oneOperand "DEC" r
formatInstruction _dstate (ADD r) = oneOperand "ADD" r
formatInstruction _dstate (ADC r) = oneOperand "ADC" r
formatInstruction _dstate (SUB r) = oneOperand "SUB" r
formatInstruction _dstate (SBC r) = oneOperand "SBC" r
formatInstruction _dstate (AND r) = oneOperand "AND" r
formatInstruction _dstate (XOR r) = oneOperand "XOR" r
formatInstruction _dstate (OR r) = oneOperand "OR" r
formatInstruction _dstate (CP r) = oneOperand "CP" r
formatInstruction _dstate (ADDHL r) = ("ADD", BC.append "HL, " $ formatOperand r)
formatInstruction _dstate HALT = zeroOperands "HALT"
formatInstruction _dstate NOP = zeroOperands "NOP"
formatInstruction _dstate EXAFAF' = ("EX", "AF, AF'")
formatInstruction _dstate EXSPHL = ("EX", "(SP), HL")
formatInstruction _dstate EXDEHL = ("EX", "DE, HL")
formatInstruction _dstate DI = zeroOperands "DI"
formatInstruction _dstate EI = zeroOperands "EI"
formatInstruction _dstate EXX = zeroOperands "EXX"
formatInstruction _dstate JPHL = ("JP", "HL")
formatInstruction _dstate LDSPHL = ("LD", "SP, HL")
formatInstruction _dstate RLCA = zeroOperands "RLCA"
formatInstruction _dstate RRCA = zeroOperands "RRCA"
formatInstruction _dstate RLA = zeroOperands "RLA"
formatInstruction _dstate RRA = zeroOperands "RRA"
formatInstruction _dstate DAA = zeroOperands "DAA"
formatInstruction _dstate CPL = zeroOperands "CPL"
formatInstruction _dstate SCF = zeroOperands "SCF"
formatInstruction _dstate CCF = zeroOperands "CCF"
formatInstruction dstate (DJNZ addr) = oneOperandAddr "DJNZ" addr dstate
formatInstruction dstate (JR addr) = oneOperandAddr "JR" addr dstate
formatInstruction dstate (JRCC cc addr) = twoOperandAddr "JR" cc addr dstate
formatInstruction _dstate (JP addr) = oneOperand "JP" addr
formatInstruction _dstate (JPCC cc addr) = twoOperands "JP" cc addr
formatInstruction _dstate (IN port) = oneOperand "IN" port
formatInstruction _dstate (OUT port) = oneOperand "OUT" port
formatInstruction _dstate (CALL addr) = oneOperand "CALL" addr
formatInstruction _dstate (CALLCC cc addr) = twoOperands "CALL" cc addr
formatInstruction _dstate RET = zeroOperands "RET"
formatInstruction _dstate (RETCC cc) = oneOperand "RET" cc
formatInstruction _dstate (PUSH r) = oneOperand "PUSH" r
formatInstruction _dstate (POP r) = oneOperand "POP" r
formatInstruction _dstate (RST rst) = oneOperand "RST" rst

formatInstruction _dstate (RLC r) = oneOperand "RLC" r
formatInstruction _dstate (RRC r) = oneOperand "RRC" r
formatInstruction _dstate (RL r) = oneOperand "RL" r
formatInstruction _dstate (RR r) = oneOperand "RR" r
formatInstruction _dstate (SLA r) = oneOperand "SLA" r
formatInstruction _dstate (SRA r) = oneOperand "SRA" r
formatInstruction _dstate (SLL r) = oneOperand "SLL" r
formatInstruction _dstate (SRL r) = oneOperand "SRL" r
formatInstruction _dstate (BIT bit r) = twoOperands "BIT" bit r
formatInstruction _dstate (RES bit r) = twoOperands "RES" bit r
formatInstruction _dstate (SET bit r) = twoOperands "SET" bit r
formatInstruction _dstate NEG = zeroOperands "NEG"
formatInstruction _dstate RETI = zeroOperands "RETI"
formatInstruction _dstate RETN = zeroOperands "RETN"
formatInstruction _dstate (IM mode) = oneOperand "IM" mode
formatInstruction _dstate RLD = zeroOperands "RLD"
formatInstruction _dstate RRD = zeroOperands "RRD"
formatInstruction _dstate LDI = zeroOperands "LDI"
formatInstruction _dstate CPI = zeroOperands "CPI"
formatInstruction _dstate INI = zeroOperands "INI"
formatInstruction _dstate OUTI = zeroOperands "OUTI"
formatInstruction _dstate LDD = zeroOperands "LDD"
formatInstruction _dstate CPD = zeroOperands "CPD"
formatInstruction _dstate IND = zeroOperands "IND"
formatInstruction _dstate OUTD = zeroOperands "OUTD"
formatInstruction _dstate LDIR = zeroOperands "LDIR"
formatInstruction _dstate CPIR = zeroOperands "CPIR"
formatInstruction _dstate INIR = zeroOperands "INIR"
formatInstruction _dstate OTIR = zeroOperands "OTIR"
formatInstruction _dstate LDDR = zeroOperands "LDDR"
formatInstruction _dstate CPDR = zeroOperands "CPDR"
formatInstruction _dstate INDR = zeroOperands "INDR"
formatInstruction _dstate OTDR = zeroOperands "OTDR"

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

-- | Disassembly output with an instruction having one operand
oneOperandAddr :: ByteString
               -> Z80addr
               -> Z80Disassembly
               -> (ByteString, ByteString)
oneOperandAddr mne addr dstate = case Map.lookup addr (get symbolTab dstate) of
                                   Nothing    -> (mne, formatOperand addr)
                                   Just label -> (mne, label)

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

-- | Disassembly output with a two operand instruction, second operand is an address
twoOperandAddr :: forall operand1. (DisOperandFormat operand1) =>
                  ByteString
               -> operand1
               -> Z80addr
               -> Z80Disassembly
            -> (ByteString, ByteString)
twoOperandAddr mne op1 addr dstate = (mne, BC.concat [ formatOperand op1
                                                     , ", "
                                                     , (case Map.lookup addr (get symbolTab dstate) of
                                                          Nothing    -> formatOperand addr
                                                          Just label -> label)
                                                     ]
                                     )

-- | Output an accumulator load or store
accumLoadStore :: AccumLoadStore                -- ^ Operand to output
               -> Bool                          -- ^ True = store, False = load
               -> ByteString
accumLoadStore operand loadStore =
  let arg1 = case operand of
               BCIndirect           -> "(BC)"
               DEIndirect           -> "(DE)"
               (Imm16Indirect addr) -> BC.concat [ "("
                                                 , formatOperand addr
                                                 , ")"
                                                 ]
               IReg                 -> "I"
               RReg                 -> "R"
  in  if loadStore then
        BC.append arg1 ", A"
      else
        BC.append "A, " arg1

-- | Output a 16-bit register indirect load/store
indirect16LoadStore :: RegPairSP
                    -> Z80addr
                    -> Bool
                    -> ByteString
indirect16LoadStore rp addr loadStore =
  let rp'   = formatOperand rp
      addr' = formatOperand addr
  in  if loadStore then
        BC.concat [ "("
                  , addr'
                  , "), "
                  , rp'
                  ]
      else
        BC.concat [ rp'
                  , ", ("
                  , addr'
                  , ")"
                  ]

class DisOperandFormat x where
  formatOperand :: x -> ByteString

instance DisOperandFormat Z80word where
  formatOperand = oldStyleHex

instance DisOperandFormat Z80addr where
  formatOperand = oldStyleHex

instance DisOperandFormat OperLD8 where
  formatOperand (Reg8Reg8 r r') = BC.append (formatOperand r) (BC.append ", " (formatOperand r'))
  formatOperand (Reg8Imm r imm) = BC.append (formatOperand r) (BC.append ", " (formatOperand imm))
  formatOperand (HLIndLoad r)   = BC.append (formatOperand r) ", (HL)"
  formatOperand (IXIndLoad r disp) = BC.append (formatOperand r) (BC.append ", (IX" (BC.append (showDisp disp) ")"))
  formatOperand (IYIndLoad r disp) = BC.append (formatOperand r) (BC.append ", (IY" (BC.append (showDisp disp) ")"))

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

formatPseudo :: Z80PseudoOps
             -> Z80Disassembly
             -> ByteString
             -> ByteString

formatPseudo (ByteRange sAddr bytes) dstate acc = 
  let outF vec =  BC.concat [ padTo lenMnemonic "DB"
                            , BC.intercalate ", " [ oldStyleHex x | x <- DVU.toList vec ]
                            ]
  in  BC.append (fmtByteGroup dstate bytes sAddr 0 outF) acc

formatPseudo (AsciiZ sAddr str) dstate acc =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      nonNullSlice  = DVU.slice 0 (DVU.length str - 1) str
      mkString      = BC.cons '\'' (BC.snoc (BC.pack [ (chr . fromIntegral) x | x <- DVU.toList nonNullSlice ]) '\'')
      outF _vec     = BC.empty
  in  BC.concat [ formatLinePrefix initSlice sAddr dstate
                , padTo lenMnemonic "DZ"
                , mkString
                , "\n"
                , fmtByteGroup dstate str (sAddr + 8) 8 outF
                , acc
                ]

formatPseudo (Ascii sAddr str) dstate acc =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      mkString      = BC.cons '\'' (BC.snoc (BC.pack [ (chr . fromIntegral) x | x <- DVU.toList str ]) '\'')
      outF _vec     = BC.empty
  in  BC.concat [ formatLinePrefix initSlice sAddr dstate
                , padTo lenMnemonic "DS"
                , mkString
                , "\n"
                , fmtByteGroup dstate str (sAddr + 8) 8 outF
                , acc
                ]

formatPseudo (Equate _saddr _label) _dstate acc = acc

-- | Format groups of bytes by groups of 8
fmtByteGroup :: Z80Disassembly
             -> Vector Z80word
             -> Z80addr
             -> Int
             -> (Vector Z80word -> ByteString)
             -> ByteString
fmtByteGroup dstate bytes addr idx outF
  | idx >= DVU.length bytes =
    BC.empty
  | idx + 8 >= DVU.length bytes =
    -- dump remaining bytes
    BC.concat [ formatLinePrefix (DVU.slice idx (DVU.length bytes - idx) bytes) addr dstate
              , outF (DVU.slice idx (DVU.length bytes - idx) bytes)
              , "\n"
              ]
  | otherwise =
    -- dump a group of 8 bytes
    BC.concat [ formatLinePrefix (DVU.slice idx 8 bytes) addr dstate
              , outF (DVU.slice idx 8 bytes)
              , "\n"
              , fmtByteGroup dstate bytes (addr + 8) (idx + 8) outF
              ]

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

showDisp :: Z80word
         -> ByteString
showDisp disp
  | disp < 0  = BC.pack . show $ disp
  | otherwise = BC.append "+" (BC.pack . show $ disp)