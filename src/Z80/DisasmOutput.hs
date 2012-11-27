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
outputDisassembly dstate = BC.append (doOutput (viewl (get disasmSeq dstate)) BC.empty)
                                     (formatSymTab (get symbolTab dstate))
  where
    -- | Recurse through the left view
    doOutput EmptyL acc = acc
    doOutput (left :< rest) acc = formatElem left (doOutput (viewl rest) acc)

    formatElem thing acc = BC.append ( case thing of
                                         (DisasmInst addr bytes ins) -> BC.concat [ formatLinePrefix bytes addr dstate
                                                                                  , formatIns ins
                                                                                  , "\n"
                                                                                  ]
                                         (DisasmPseudo pseudo)       -> formatPseudo pseudo dstate
                                     )
                                     acc

    formatIns ins = let (mnemonic, opers) = formatInstruction dstate ins
                    in  padTo lenInstruction $ BC.append (padTo lenMnemonic mnemonic) opers

    formatSymTab symTab = let maxsym    = maxSymLen symTab
                              totalCols = lenOutputLine `div` (maxsym + extraSymPad)
                              (_, symsByAddr) = Map.foldlWithKey (symbolsByAddr maxsym totalCols) (0, BC.empty) symTab
                              (_, symsByName) = Map.foldlWithKey (symbolsByName maxsym totalCols) (0, BC.empty)
                                                                 (Map.fromList [ (y, x) | (x, y) <- Map.assocs symTab ])
                          in  BC.concat [ "\n\n"
                                        , "Symbol Table (alpha):"
                                        , "\n"
                                        , symsByName
                                        , "\n\n"
                                        , "Symbol Table (numeric):"
                                        , "\n"
                                        , symsByAddr
                                        ]

    -- Extra symbol padding: 4 for the hex address, 3 for " = " and 2 for intercolumn spacing
    extraSymPad = 4 + 3 + 2

    -- Find maximum symbol name length. Note: Curried form, assumes last parameter is the symbol table (Map Z80addr ByteString)
    maxSymLen = Map.foldl' (\len str -> if len < BC.length str; then BC.length str; else len) 0

    -- Dump the symbols by address, columnar format
    symbolsByAddr maxsym nColumns (col, syms) kSym sym =
      let (newCol, delim) = if col >= nColumns then
                              (0, "\n")
                            else
                              (col + 1, "  ")
      in ( newCol,
           BC.concat [ syms
                     , padTo maxsym sym
                     , " = "
                     , upperHex kSym
                     , delim
                     ]
         )

    -- Dump the symbols by name, columnar format
    symbolsByName maxsym nColumns (col, syms) theSym symAddr =
      let (newCol, delim) = if col >= nColumns then
                              (0, "\n")
                            else
                              (col + 1, "  ")
      in ( newCol,
           BC.concat [ syms
                     , padTo maxsym theSym
                     , " = "
                     , upperHex symAddr
                     , delim
                     ]
         )

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
                     , padTo lenInsBytes ""
                     , "|"
                     , padTo lenAsChars ""
                     , "| "
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

-- | Length of instruction opcode output: 8 bytes max to dump, 3 spaces per byte
lenInsBytes :: Int64
lenInsBytes = (lenAsChars * 3)
-- | Length of opcode-as-characters output: 8 characters, as the bytes are dumped as characters
lenAsChars :: Int64
lenAsChars = 8
-- | Length of the label colum: 8 characters plus ": "
lenSymLabel :: Int64
lenSymLabel = BC.length("XXXXXXXXXXXX") + 2
-- | Total length of the instruction+operands output
lenInstruction :: Int64
lenInstruction = 24
-- | Length of the instruction mnemonic
lenMnemonic :: Int64
lenMnemonic = 6
-- | Length of the output address
lenAddress :: Int64
lenAddress = 4 + 2                              -- "XXXX" ++ ": "
-- | Length of the line's prefix
lenOutputPrefix :: Int64
lenOutputPrefix = lenAddress + lenInsBytes + lenAsChars + 3
-- | Total length of each output line:
lenOutputLine :: Int64
lenOutputLine = lenOutputPrefix + lenSymLabel + lenInstruction

-- | The main workhourse of this module: Format a 'Z80instruction' as the tuple '(mnemonic, operands)'
formatInstruction :: Z80Disassembly             -- ^ Disassembly state, used to grab the disassembly symbol table
                  -> Z80instruction             -- ^ Instruction to format
                  -> (ByteString, ByteString)   -- ^ '(mnemonic, operands)' result tuple

formatInstruction _dstate (Z80undef _)            = zeroOperands "???"
formatInstruction _dstate (LD8 x)                 = oneOperand "LD" x
formatInstruction _dstate (LDA x)                 = ("LD", (accumLoadStore x False))
formatInstruction _dstate (STA x)                 = ("LD", (accumLoadStore x True))
formatInstruction _dstate (LD16 r imm)            = twoOperands "LD" r imm
formatInstruction _dstate (STHL addr)             = ("LD", BC.append "(" (BC.append (formatOperand addr) "), HL"))
formatInstruction _dstate (LDHL addr)             = ("LD", BC.append "HL, (" (BC.append (formatOperand addr) ")"))
formatInstruction _dstate (LD16Indirect rp addr)  = ("LD", indirect16LoadStore rp addr False)
formatInstruction _dstate (ST16Indirect addr rp)  = ("LD", indirect16LoadStore rp addr True)
formatInstruction _dstate (INC r)                 = oneOperand "INC" r
formatInstruction _dstate (DEC r)                 = oneOperand "DEC" r
formatInstruction _dstate (INC16 r)               = oneOperand "INC" r
formatInstruction _dstate (DEC16 r)               = oneOperand "DEC" r
formatInstruction _dstate (ADD r)                 = oneOperand "ADD" r
formatInstruction _dstate (ADC r)                 = oneOperand "ADC" r
formatInstruction _dstate (SUB r)                 = oneOperand "SUB" r
formatInstruction _dstate (SBC r)                 = oneOperand "SBC" r
formatInstruction _dstate (AND r)                 = oneOperand "AND" r
formatInstruction _dstate (XOR r)                 = oneOperand "XOR" r
formatInstruction _dstate (OR r)                  = oneOperand "OR" r
formatInstruction _dstate (CP r)                  = oneOperand "CP" r
formatInstruction _dstate HALT                    = zeroOperands "HALT"
formatInstruction _dstate NOP                     = zeroOperands "NOP"
formatInstruction _dstate EXAFAF'                 = ("EX", "AF, AF'")
formatInstruction _dstate EXSPHL                  = ("EX", "(SP), HL")
formatInstruction _dstate EXDEHL                  = ("EX", "DE, HL")
formatInstruction _dstate DI                      = zeroOperands "DI"
formatInstruction _dstate EI                      = zeroOperands "EI"
formatInstruction _dstate EXX                     = zeroOperands "EXX"
formatInstruction _dstate JPHL                    = ("JP", "HL")
formatInstruction _dstate LDSPHL                  = ("LD", "SP, HL")
formatInstruction _dstate RLCA                    = zeroOperands "RLCA"
formatInstruction _dstate RRCA                    = zeroOperands "RRCA"
formatInstruction _dstate RLA                     = zeroOperands "RLA"
formatInstruction _dstate RRA                     = zeroOperands "RRA"
formatInstruction _dstate DAA                     = zeroOperands "DAA"
formatInstruction _dstate CPL                     = zeroOperands "CPL"
formatInstruction _dstate SCF                     = zeroOperands "SCF"
formatInstruction _dstate CCF                     = zeroOperands "CCF"
formatInstruction _dstate (DJNZ addr)             = oneOperand "DJNZ" addr
formatInstruction _dstate (JR addr)               = oneOperand "JR" addr
formatInstruction _dstate (JRCC cc addr)          = twoOperands "JR" cc addr
formatInstruction _dstate (JP addr)               = oneOperand "JP" addr 
formatInstruction _dstate (JPCC cc addr)          = twoOperands "JP" cc addr
formatInstruction _dstate (IN port)               = ("IN", ioPortOperand port True)
formatInstruction _dstate (OUT port)              = ("OUT", ioPortOperand port False)
formatInstruction _dstate (CALL addr)             = oneOperand "CALL" addr
formatInstruction _dstate (CALLCC cc addr)        = twoOperands "CALL" cc addr
formatInstruction _dstate RET                     = zeroOperands "RET"
formatInstruction _dstate (RETCC cc)              = oneOperand "RET" cc
formatInstruction _dstate (PUSH r)                = oneOperand "PUSH" r
formatInstruction _dstate (POP r)                 = oneOperand "POP" r
formatInstruction _dstate (RST rst)               = oneOperand "RST" rst

formatInstruction _dstate (RLC r)                 = oneOperand "RLC" r
formatInstruction _dstate (RRC r)                 = oneOperand "RRC" r
formatInstruction _dstate (RL r)                  = oneOperand "RL" r
formatInstruction _dstate (RR r)                  = oneOperand "RR" r
formatInstruction _dstate (SLA r)                 = oneOperand "SLA" r
formatInstruction _dstate (SRA r)                 = oneOperand "SRA" r
formatInstruction _dstate (SLL r)                 = oneOperand "SLL" r
formatInstruction _dstate (SRL r)                 = oneOperand "SRL" r
formatInstruction _dstate (BIT bit r)             = twoOperands "BIT" bit r
formatInstruction _dstate (RES bit r)             = twoOperands "RES" bit r
formatInstruction _dstate (SET bit r)             = twoOperands "SET" bit r
formatInstruction _dstate NEG                     = zeroOperands "NEG"
formatInstruction _dstate RETI                    = zeroOperands "RETI"
formatInstruction _dstate RETN                    = zeroOperands "RETN"
formatInstruction _dstate (IM mode)               = oneOperand "IM" mode
formatInstruction _dstate RLD                     = zeroOperands "RLD"
formatInstruction _dstate RRD                     = zeroOperands "RRD"
formatInstruction _dstate LDI                     = zeroOperands "LDI"
formatInstruction _dstate CPI                     = zeroOperands "CPI"
formatInstruction _dstate INI                     = zeroOperands "INI"
formatInstruction _dstate OUTI                    = zeroOperands "OUTI"
formatInstruction _dstate LDD                     = zeroOperands "LDD"
formatInstruction _dstate CPD                     = zeroOperands "CPD"
formatInstruction _dstate IND                     = zeroOperands "IND"
formatInstruction _dstate OUTD                    = zeroOperands "OUTD"
formatInstruction _dstate LDIR                    = zeroOperands "LDIR"
formatInstruction _dstate CPIR                    = zeroOperands "CPIR"
formatInstruction _dstate INIR                    = zeroOperands "INIR"
formatInstruction _dstate OTIR                    = zeroOperands "OTIR"
formatInstruction _dstate LDDR                    = zeroOperands "LDDR"
formatInstruction _dstate CPDR                    = zeroOperands "CPDR"
formatInstruction _dstate INDR                    = zeroOperands "INDR"
formatInstruction _dstate OTDR                    = zeroOperands "OTDR"

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

{-
-- | Disassembly output with an instruction having one operand
oneOperandAddr :: ByteString
               -> OperAddr
               -> Z80Disassembly
               -> (ByteString, ByteString)
oneOperandAddr mne addr dstate = case addr of
                                   (AbsAddr absAddr) -> case Map.lookup absAddr (get symbolTab dstate) of
                                                          Nothing    -> (mne, formatOperand absAddr)
                                                          Just label -> (mne, label)
                                   (SymAddr label)   -> (mne, label)
-}

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

{-
-- | Disassembly output with a two operand instruction, second operand is an address
twoOperandAddr :: forall operand1. (DisOperandFormat operand1) =>
                  ByteString
               -> operand1
               -> OperAddr
               -> Z80Disassembly
            -> (ByteString, ByteString)
twoOperandAddr mne op1 addr dstate = (mne, BC.concat [ formatOperand op1
                                                     , ", "
                                                     , (case addr of 
                                                          (AbsAddr absAddr) -> case Map.lookup absAddr (get symbolTab dstate) of
                                                                                 Nothing    -> formatOperand absAddr
                                                                                 Just label -> label
                                                          (SymAddr label)   -> label
                                                       )
                                                     ]
                                     )
-}

-- | Output an accumulator load or store
accumLoadStore :: AccumLoadStore                -- ^ Operand to output
               -> Bool                          -- ^ True = store, False = load
               -> ByteString
accumLoadStore operand isStore =
  let arg1 = case operand of
               BCIndirect           -> "(BC)"
               DEIndirect           -> "(DE)"
               (Imm16Indirect addr) -> BC.cons '(' $ BC.snoc (formatOperand addr) ')'
               IReg                 -> "I"
               RReg                 -> "R"
  in  if isStore then
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

-- | Output an I/O port operand
ioPortOperand :: OperIO                         -- ^ Operand
              -> Bool                           -- ^ 'True' means an IN instruction, 'False' means an OUT instruction
              -> ByteString                     -- ^ Operand string
ioPortOperand port inOut = case port of
                             (PortImm imm)  -> formatOperand imm
                             (CIndIO reg8)  -> if inOut then
                                                 BC.append (formatOperand reg8) ", (C)"
                                               else
                                                 BC.append "(C), " (formatOperand reg8)
                             CIndIO0        -> if inOut then
                                                 "(C)"
                                               else
                                                 "(C), 0"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

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

instance DisOperandFormat OperExtendedALU where
  formatOperand (ALU8 op)  = formatOperand op
  formatOperand (ALU16 rp) = BC.append "HL, " (formatOperand rp)

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
  formatOperand (IXindirect disp) = BC.concat ["(IX"
                                              , showDisp disp
                                              , ")"
                                              ]
  formatOperand (IYindirect disp) = BC.concat [ "(IY"
                                              , showDisp disp
                                              , ")"
                                              ]

instance DisOperandFormat Z80reg16 where
  formatOperand BC = "BC"
  formatOperand DE = "DE"
  formatOperand HL = "HL"
  formatOperand IX = "IX"
  formatOperand IY = "IY"

instance DisOperandFormat Z80condC where
  formatOperand NZ   = "NZ"
  formatOperand Z    = "Z"
  formatOperand NC   = "NC"
  formatOperand CY   = "C"
  formatOperand PO   = "PO"
  formatOperand PE   = "PE"
  formatOperand POS  = "P"
  formatOperand MI   = "M"

instance DisOperandFormat RegPairAF where
  formatOperand (RPair16' r) = formatOperand r
  formatOperand AF = "AF"

instance DisOperandFormat OperAddr where
  formatOperand (AbsAddr addr) = formatOperand addr
  formatOperand (SymAddr label) = label

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

formatPseudo :: Z80PseudoOps
             -> Z80Disassembly
             -> ByteString

formatPseudo (ByteRange sAddr bytes) dstate = 
  let outF vec =  BC.concat [ padTo lenMnemonic "DB"
                            , BC.intercalate ", " [ oldStyleHex x | x <- DVU.toList vec ]
                            ]
  in  fmtByteGroup dstate bytes sAddr 0 outF

formatPseudo (ByteExpression addr expr word) dstate = 
  let outF _vec = BC.concat [ padTo lenMnemonic "DB"
                              , if (not . BC.null) expr then
                                  expr
                                else
                                  upperHex word
                            ]
  in  fmtByteGroup dstate (DVU.singleton word) addr 0 outF

formatPseudo (AddrWord sAddr addr bytes) dstate =
  BC.concat [ formatLinePrefix bytes sAddr dstate
            , padTo lenMnemonic "DA"
            , formatOperand addr
            , "\n"
            ]

formatPseudo (AsciiZ sAddr str) dstate =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      nonNullSlice  = DVU.slice 0 (DVU.length str - 1) str
      mkString      = BC.cons '\'' (BC.snoc (BC.pack [ (chr . fromIntegral) x | x <- DVU.toList nonNullSlice ]) '\'')
      outF _vec     = BC.empty
  in  BC.concat [ formatLinePrefix initSlice sAddr dstate
                , padTo lenMnemonic "DZ"
                , mkString
                , "\n"
                , fmtByteGroup dstate str (sAddr + 8) 8 outF
                ]

formatPseudo (Ascii sAddr str) dstate =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      mkString      = BC.cons '\'' (BC.snoc (BC.pack [ (chr . fromIntegral) x | x <- DVU.toList str ]) '\'')
      outF _vec     = BC.empty
  in  BC.concat [ formatLinePrefix initSlice sAddr dstate
                , padTo lenMnemonic "DS"
                , mkString
                , "\n"
                , fmtByteGroup dstate str (sAddr + 8) 8 outF
                ]

formatPseudo (DisOrigin origin) _dstate   = BC.concat [ BC.replicate (lenOutputPrefix + lenSymLabel) ' '
                                                      , padTo lenMnemonic "ORG"
                                                      , asHex origin
                                                      , "\n"
                                                      ]

formatPseudo (AddrEquate label addr) _dstate = BC.concat [ BC.replicate lenOutputPrefix ' '
                                                         , padTo lenSymLabel label
                                                         , padTo lenMnemonic "="
                                                         , upperHex addr
                                                         , "\n"
                                                         ]

formatPseudo (LineComment comment) _dstate = BC.concat [ BC.replicate lenOutputPrefix ' '
                                                       , "; "
                                                       , comment
                                                       , "\n"
                                                       ]

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

showDisp :: Z80disp
         -> ByteString
showDisp disp
  | disp < 0  = BC.pack . show $ disp
  | otherwise = BC.append "+" (BC.pack . show $ disp)
