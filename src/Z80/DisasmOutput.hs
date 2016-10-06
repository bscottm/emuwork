{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Z80 disassmbler output.
--
-- Disassembler output consists of two elements: the "analytic" part (address, opcode bytes and ASCII) and the "assembler" part.
-- The assembler part is Misosys EDAS-compatible, so, if the "analytic" part is stripped from the output, the resulting "assembler"
-- part could be fed into the Misosys-EDAS compatible assembler to regenerate the object code.
module Z80.DisasmOutput
  ( z80AnalyticDisassembly
  , z80AnalyticDisassemblyOutput
  ) where

-- import Debug.Trace

import System.IO
import Data.Char
import Data.Tuple
import Data.Generics.Aliases
import Data.Generics.Schemes
import Control.Lens hiding ((<|), (|>))
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as DVU

import Machine
import Z80.InstructionSet
import Z80.Processor
import Z80.Disassembler

-- | Format the "analytic" version of the disassembled Z80 sequence as a 'Text'
-- sequence. This outputs the address, opcode bytes, the opcodes as ASCII,
-- followed by the standard \'label: instruction ; comment\' output.
z80AnalyticDisassembly :: Z80disassembly
                       -> Seq T.Text
z80AnalyticDisassembly dstate =
  -- Append the symbol table to the formatted instruction sequence
  -- Unfortunately (maybe not...?) the foldl results in the concatenation of many singletons.
  dstate ^. symbolTab & formatSymTab $ formattedDisSeq dstate
  where
    formattedDisSeq z80dstate = (fixupSymbols z80dstate) ^. disasmSeq & Foldable.foldl formatElem Seq.empty
    formatElem accSeq (DisasmInsn addr bytes ins cmnt) = accSeq >< formatLinePrefix bytes addr (formatIns ins cmnt)
    formatElem accSeq pseudo                           = accSeq >< formatPseudo pseudo

-- | Generate the "analytic" version of the output (opcodes, ASCII representation) and output to an 'IO' handle.
z80AnalyticDisassemblyOutput :: Handle
                             -> Z80disassembly
                             -> IO ()
z80AnalyticDisassemblyOutput hOut dstate = Foldable.traverse_ (TIO.hPutStrLn hOut) $ z80AnalyticDisassembly dstate

-- | Pass over the disassembly sequence, translating absolute addresses into symbolic addresses.
fixupSymbols :: Z80disassembly
             -> Z80disassembly
fixupSymbols z80dstate =
  let symtab = z80dstate ^. symbolTab
      -- Translate an absolute address, generally hidden inside an instruction operand, into a symbolic address
      -- if present in the symbol table.
      fixupSymbol addr@(AbsAddr absAddr)    =
        case absAddr `Map.lookup` symtab of
          Nothing      -> addr
          Just symName -> SymAddr symName
      fixupSymbol symAddr                   = symAddr

      -- Lookup address labels in the symbol table
      fixupEltAddress plain@(Plain absAddr) =
        case absAddr `Map.lookup` symtab of
          Nothing      -> plain
          Just symName -> Labeled absAddr symName
      fixupEltAddress labeled               = labeled
  in  -- Note the cool use of composed functions in a SYB transformation!
      disasmSeq %~ (everywhere $ (mkT fixupSymbol) . (mkT fixupEltAddress)) $ z80dstate

-- | Format the accumulated symbol table as a sequence of 'T.Text's, in columnar format
formatSymTab :: Map Z80addr T.Text
             -> Seq T.Text
             -> Seq T.Text
formatSymTab symTab outSeq =
  let maxsym     = Map.foldl' (\len str -> if len < T.length str; then T.length str; else len) 0 symTab
      totalCols  = fromIntegral(((lenOutputLine - maxsym) `div` (maxsym + extraSymPad)) + 1) :: Int
      byNameSyms =  Map.fromList $ map swap $ Map.assocs symTab
      byAddrSeq = T.empty
                  <| T.empty
                  <| "Symbol Table (numeric):"
                  <| T.empty
                  <| (columnar$ Map.foldlWithKey formatSymbol Seq.empty symTab)
      byNameSeq = T.empty
                   <| T.empty
                   <| "Symbol Table (alpha):"
                   <| T.empty
                   <| (columnar $ Map.foldlWithKey (\acc sym addr -> formatSymbol acc addr sym) Seq.empty byNameSyms)
      -- Consolidate sequence into columnar format
      columnar symSeq = if Seq.length symSeq >= totalCols then 
                          let (thisCol, rest) = Seq.splitAt totalCols symSeq
                          in  (T.intercalate "  " $ Foldable.toList thisCol) <| (columnar rest)
                        else
                          Seq.singleton $ T.intercalate "  " $ Foldable.toList symSeq

      -- Extra symbol padding: 4 for the hex address, 3 for " = " and 2 for intercolumn spacing
      extraSymPad = 4 + 3 + 2

      -- Format the symbol:
      formatSymbol theSeq addr sym = theSeq |> T.concat [ padTo maxsym sym
                                                         , " = "
                                                         , upperHex addr
                                                         ]
    in  outSeq >< byNameSeq >< byAddrSeq

-- | Format a Z80 instruction
formatIns :: Z80instruction             -- ^ Instruction to format
          -> T.Text                     -- ^ Optional appended comment
          -> T.Text                     -- ^ Formatted result
formatIns ins cmnt = let (mnemonic, opers) = formatInstruction ins
                         cmnt'             = if (not . T.null) cmnt then
                                               T.append "; " cmnt
                                             else
                                               T.empty
                     in  T.append (padTo lenInstruction $ T.append (padTo lenMnemonic mnemonic) opers) cmnt'

-- | Output a formatted address in uppercase hex
upperHex :: ShowHex operand => 
            operand
         -> T.Text
upperHex = makeUpper . asHex

-- | Output an 'upperHex' address with the appended 'H'
oldStyleHex :: ShowHex operand =>
               operand
            -> T.Text
oldStyleHex x = T.snoc (upperHex x) 'H'

-- | Format the beginning of the line (address, bytes, label, etc.)
formatLinePrefix :: Vector Z80word              -- ^ Opcode vector
                 -> DisEltAddress Z80addr       -- ^ Address of this output line
                 -> T.Text                      -- ^ Output to emit (formatted instruction, ...)
                 -> Seq T.Text                  -- ^ Resulting 'T.Text' output sequence
formatLinePrefix bytes addr outString =
  let addrLabel           = disEltLabel addr
      label               = if (not . T.null) addrLabel then
                              addrLabel `T.snoc` ':'
                            else
                              addrLabel
      bytesToChars vec    = padTo lenAsChars $ DVU.foldl (\s x -> T.append s (mkPrintable x)) T.empty vec
      mkPrintable x       = if x > 0x20 && x < 0x7f then (T.singleton . chr . fromIntegral) x; else " "
      linePrefix          = T.concat [ upperHex (disEltAddress addr)
                                      , ": "
                                      , formatBytes bytes
                                      , "|"
                                      , bytesToChars bytes
                                      , "| "
                                      ]
  in  if T.length label < (lenSymLabel - 2) then
        Seq.singleton $ T.concat [ linePrefix
                                  , padTo lenSymLabel label
                                  , outString
                                  ]
      else
        ( Seq.singleton $ T.concat [ upperHex (disEltAddress addr)
                                    , ": "
                                    , T.replicate lenInsBytes textSpace
                                    , "|"
                                    , T.replicate lenAsChars textSpace
                                    , "| "
                                    , label
                                    ]
        )
        |> ( T.concat [ linePrefix
                       , T.replicate lenSymLabel textSpace
                       , outString
                       ]
             )

-- | Format a series of bytes
formatBytes :: Vector Z80word
            -> T.Text
formatBytes bytes = padTo lenInsBytes $ T.intercalate " " [ upperHex x | x <- DVU.toList bytes ]

-- Lengths of various output columns:

-- | Length of instruction opcode output: 8 bytes max to dump, 3 spaces per byte
lenInsBytes :: Int
lenInsBytes = (lenAsChars * 3)
-- | Length of opcode-as-characters output: 8 characters, as the bytes are dumped as characters
lenAsChars :: Int
lenAsChars = 8
-- | Length of the label colum: 8 characters plus ": "
lenSymLabel :: Int
lenSymLabel = T.length("XXXXXXXXXXXX") + 2
-- | Total length of the instruction+operands output
lenInstruction :: Int
lenInstruction = 24
-- | Length of the instruction mnemonic
lenMnemonic :: Int
lenMnemonic = 6
-- | Length of the output address
lenAddress :: Int
lenAddress = 4 + 2                              -- "XXXX" ++ ": "
-- | Length of the line's prefix
lenOutputPrefix :: Int
lenOutputPrefix = lenAddress + lenInsBytes + lenAsChars + 3
-- | Total length of each output line:
lenOutputLine :: Int
lenOutputLine = lenOutputPrefix + lenSymLabel + lenInstruction

-- | The main workhourse of this module: Format a 'Z80instruction' as the tuple '(mnemonic, operands)'
formatInstruction :: Z80instruction             -- ^ Instruction to format
                  -> (T.Text, T.Text)           -- ^ '(mnemonic, operands)' result tuple

formatInstruction (Z80undef _)            = zeroOperands "???"
formatInstruction (LD x)                  = oneOperand "LD" x
formatInstruction (INC r)                 = oneOperand "INC" r
formatInstruction (DEC r)                 = oneOperand "DEC" r
formatInstruction (INC16 r)               = oneOperand "INC" r
formatInstruction (DEC16 r)               = oneOperand "DEC" r
formatInstruction (ADD r)                 = oneOperand "ADD" r
formatInstruction (ADC r)                 = oneOperand "ADC" r
formatInstruction (SUB r)                 = oneOperand "SUB" r
formatInstruction (SBC r)                 = oneOperand "SBC" r
formatInstruction (AND r)                 = oneOperand "AND" r
formatInstruction (XOR r)                 = oneOperand "XOR" r
formatInstruction (OR r)                  = oneOperand "OR" r
formatInstruction (CP r)                  = oneOperand "CP" r
formatInstruction HALT                    = zeroOperands "HALT"
formatInstruction NOP                     = zeroOperands "NOP"
formatInstruction (EXC AFAF')             = ("EX", "AF, AF'")
formatInstruction (EXC SPHL)              = ("EX", "(SP), HL")
formatInstruction (EXC DEHL)              = ("EX", "DE, HL")
formatInstruction DI                      = zeroOperands "DI"
formatInstruction EI                      = zeroOperands "EI"
formatInstruction (EXC Primes)            = zeroOperands "EXX"
formatInstruction JPHL                    = ("JP", "HL")
formatInstruction LDSPHL                  = ("LD", "SP, HL")
formatInstruction RLCA                    = zeroOperands "RLCA"
formatInstruction RRCA                    = zeroOperands "RRCA"
formatInstruction RLA                     = zeroOperands "RLA"
formatInstruction RRA                     = zeroOperands "RRA"
formatInstruction DAA                     = zeroOperands "DAA"
formatInstruction CPL                     = zeroOperands "CPL"
formatInstruction SCF                     = zeroOperands "SCF"
formatInstruction CCF                     = zeroOperands "CCF"
formatInstruction (DJNZ addr)             = oneOperand "DJNZ" addr
formatInstruction (JR addr)               = oneOperand "JR" addr
formatInstruction (JRCC cc addr)          = twoOperands "JR" cc addr
formatInstruction (JP addr)               = oneOperand "JP" addr 
formatInstruction (JPCC cc addr)          = twoOperands "JP" cc addr
formatInstruction (IN port)               = ("IN", ioPortOperand port True)
formatInstruction (OUT port)              = ("OUT", ioPortOperand port False)
formatInstruction (CALL addr)             = oneOperand "CALL" addr
formatInstruction (CALLCC cc addr)        = twoOperands "CALL" cc addr
formatInstruction RET                     = zeroOperands "RET"
formatInstruction (RETCC cc)              = oneOperand "RET" cc
formatInstruction (PUSH r)                = oneOperand "PUSH" r
formatInstruction (POP r)                 = oneOperand "POP" r
formatInstruction (RST rst)               = ("RST", (upperHex rst))
formatInstruction (RLC r)                 = oneOperand "RLC" r
formatInstruction (RRC r)                 = oneOperand "RRC" r
formatInstruction (RL r)                  = oneOperand "RL" r
formatInstruction (RR r)                  = oneOperand "RR" r
formatInstruction (SLA r)                 = oneOperand "SLA" r
formatInstruction (SRA r)                 = oneOperand "SRA" r
formatInstruction (SLL r)                 = oneOperand "SLL" r
formatInstruction (SRL r)                 = oneOperand "SRL" r
formatInstruction (BIT bit r)             = twoOperands "BIT" bit r
formatInstruction (RES bit r)             = twoOperands "RES" bit r
formatInstruction (SET bit r)             = twoOperands "SET" bit r
formatInstruction NEG                     = zeroOperands "NEG"
formatInstruction RETI                    = zeroOperands "RETI"
formatInstruction RETN                    = zeroOperands "RETN"
formatInstruction (IM mode)               = oneOperand "IM" mode
formatInstruction RLD                     = zeroOperands "RLD"
formatInstruction RRD                     = zeroOperands "RRD"
formatInstruction LDI                     = zeroOperands "LDI"
formatInstruction CPI                     = zeroOperands "CPI"
formatInstruction INI                     = zeroOperands "INI"
formatInstruction OUTI                    = zeroOperands "OUTI"
formatInstruction LDD                     = zeroOperands "LDD"
formatInstruction CPD                     = zeroOperands "CPD"
formatInstruction IND                     = zeroOperands "IND"
formatInstruction OUTD                    = zeroOperands "OUTD"
formatInstruction LDIR                    = zeroOperands "LDIR"
formatInstruction CPIR                    = zeroOperands "CPIR"
formatInstruction INIR                    = zeroOperands "INIR"
formatInstruction OTIR                    = zeroOperands "OTIR"
formatInstruction LDDR                    = zeroOperands "LDDR"
formatInstruction CPDR                    = zeroOperands "CPDR"
formatInstruction INDR                    = zeroOperands "INDR"
formatInstruction OTDR                    = zeroOperands "OTDR"

-- formatInstruction _ = zeroOperands "--!!"

-- | Disassembly output with an instruction having no operands
zeroOperands :: T.Text 
             -> (T.Text, T.Text)
zeroOperands mne = (mne, T.empty)

-- | Disassembly output with an instruction having one operand
oneOperand :: DisOperandFormat operand =>
              T.Text
           -> operand
           -> (T.Text, T.Text)
oneOperand mne opnd = (mne, formatOperand opnd)

-- | Disassembly output with a two operand instruction
twoOperands :: (DisOperandFormat operand1, DisOperandFormat operand2) =>
               T.Text
            -> operand1
            -> operand2
            -> (T.Text, T.Text)
twoOperands mne op1 op2 = (mne, T.concat [ formatOperand op1
                                          , ", "
                                          , formatOperand op2
                                          ]
                          )

-- | Output an I/O port operand
ioPortOperand :: OperIO         -- ^ Operand
              -> Bool           -- ^ 'True' means an IN instruction, 'False' means an OUT instruction
              -> T.Text         -- ^ Operand string
ioPortOperand port inOut = case port of
                             (PortImm imm)  -> formatOperand imm
                             (CIndIO reg8)  -> if inOut then
                                                 T.append (formatOperand reg8) ", (C)"
                                               else
                                                 T.append "(C), " (formatOperand reg8)
                             CIndIO0        -> if inOut then
                                                 "(C)"
                                               else
                                                 "(C), 0"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassembler operand format function class. Make life easy on ourselves when
-- formatting assembler operands.
class DisOperandFormat x where
  -- | Convert an operand into its appropriate representation
  formatOperand :: x -> T.Text

instance DisOperandFormat Z80word where
  formatOperand = oldStyleHex

instance DisOperandFormat Z80addr where
  formatOperand = oldStyleHex

instance DisOperandFormat OperLD where
  formatOperand (Reg8Reg8 r r')           = T.append (formatOperand r) (T.append ", " (formatOperand r'))
  formatOperand (Reg8Imm r imm)           = T.append (formatOperand r) (T.append ", " (formatOperand imm))
  formatOperand AccBCIndirect             = "A, (BC)"
  formatOperand AccDEIndirect             = "A, (DE)"
  formatOperand (AccImm16Indirect addr)   = T.concat [ "A, ("
                                                     , (formatOperand addr)
                                                     , ")"
                                                     ]
  formatOperand AccIReg                   = "A, I"
  formatOperand AccRReg                   = "A, R"
  formatOperand BCIndirectStore           = "(BC), A"
  formatOperand DEIndirectStore           = "(DE), A"
  formatOperand (Imm16IndirectStore addr) = T.append (formatOperand addr) ", A"
  formatOperand IRegAcc                   = "I, A"
  formatOperand RRegAcc                   = "R, A"
  formatOperand (RPair16ImmLoad rp imm)   = T.append (T.append (formatOperand rp) ", ") (formatOperand imm)
  formatOperand (HLIndirectStore addr)    = T.concat [ "("
                                                     , (formatOperand addr)
                                                     , "), HL"
                                                     ]
  formatOperand (HLIndirectLoad  addr)    = T.concat [ "HL, ("
                                                     , (formatOperand addr)
                                                     , ")"
                                                     ]
  formatOperand (RPIndirectLoad rp addr)  = T.concat [ formatOperand rp
                                                     , ", ("
                                                     , formatOperand addr
                                                     , ")"
                                                     ]
  formatOperand (RPIndirectStore rp addr) = T.concat [ "("
                                                     , formatOperand addr
                                                     , "), "
                                                     , formatOperand rp
                                                     ]

instance DisOperandFormat OperALU where
  formatOperand (ALUimm imm)    = formatOperand imm
  formatOperand (ALUreg8 r)     = formatOperand r
  formatOperand (ALUHLindirect) = "(HL)"

instance DisOperandFormat OperExtendedALU where
  formatOperand (ALU8 opnd) = formatOperand opnd
  formatOperand (ALU16 rp)  = T.append "HL, " (formatOperand rp)

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
  formatOperand (IXindirect disp) = T.concat ["(IX"
                                              , showDisp disp
                                              , ")"
                                              ]
  formatOperand (IYindirect disp) = T.concat [ "(IY"
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

instance DisOperandFormat (SymAbsAddr Z80addr) where
  formatOperand (AbsAddr addr) = formatOperand addr
  formatOperand (SymAddr label) = label

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

formatPseudo :: Z80DisasmElt
             -> Seq T.Text

formatPseudo (ByteRange sAddr bytes) = 
  let initSlice     = DVU.slice 0 (if DVU.length bytes <= 8; then DVU.length bytes; else 8) bytes
      mkBytes vec   =  T.concat [ padTo lenMnemonic "DB"
                                , T.intercalate ", " [ oldStyleHex x | x <- DVU.toList vec ]
                                ]
  in  formatLinePrefix initSlice sAddr (mkBytes initSlice)
      >< fmtByteGroup bytes (disEltAddress sAddr + 8) 8 mkBytes

formatPseudo (ExtPseudo (ByteExpression addr expr word)) = 
  let outF _vec = T.concat [ padTo lenMnemonic "DB"
                              , if (not . T.null) expr then
                                  expr
                                else
                                  upperHex word
                            ]
  in  fmtByteGroup (DVU.singleton word) addr 0 outF

formatPseudo (Addr sAddr addr bytes) =
  formatLinePrefix bytes sAddr (T.append (padTo lenMnemonic "DA") (formatOperand addr))

formatPseudo (AsciiZ sAddr str) =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      nonNullSlice  = DVU.slice 0 (DVU.length str - 1) str
      mkString      = T.cons '\'' (T.snoc (T.pack [ (chr . fromIntegral) x | x <- DVU.toList nonNullSlice ]) '\'')
      outF _vec     = T.empty
  in  formatLinePrefix initSlice sAddr (T.append (padTo lenMnemonic "DZ") mkString)
      >< fmtByteGroup str (disEltAddress sAddr + 8) 8 outF

formatPseudo (Ascii sAddr str) =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      mkString      = T.cons '\'' (T.snoc (T.pack [ (chr . fromIntegral) x | x <- DVU.toList str ]) '\'')
      outF _vec     = T.empty
  in  formatLinePrefix initSlice sAddr (T.append (padTo lenMnemonic "DS") mkString)
      >< fmtByteGroup str (disEltAddress sAddr + 8) 8 outF

formatPseudo (DisOrigin origin) = Seq.singleton $ T.concat [ T.replicate (lenOutputPrefix + lenSymLabel) textSpace
                                                                      , padTo lenMnemonic "ORG"
                                                                      , asHex origin
                                                                      ]

formatPseudo (Equate label addr) = Seq.singleton $ T.concat [ T.replicate lenOutputPrefix textSpace
                                                                         , padTo lenSymLabel label
                                                                         , padTo lenMnemonic "="
                                                                         , upperHex addr
                                                                         ]

formatPseudo (LineComment comment) = Seq.singleton $ T.concat [ T.replicate lenOutputPrefix textSpace
                                                                       , "; "
                                                                       , comment
                                                                       ]

formatPseudo _unknownPseudo = Seq.singleton $ "[!!Unknown pseudo instruction]"

-- | Format groups of bytes by groups of 8
fmtByteGroup :: Vector Z80word
             -> Z80addr
             -> Int
             -> (Vector Z80word -> T.Text)
             -> Seq T.Text
fmtByteGroup bytes addr idx outF
  | idx >= DVU.length bytes     = Seq.empty
  | idx + 8 >= DVU.length bytes =
    -- dump remaining bytes
    let outString = outF (DVU.slice idx (DVU.length bytes - idx) bytes)
    in  formatLinePrefix (DVU.slice idx (DVU.length bytes - idx) bytes) (mkPlainAddress addr) outString
  | otherwise =
    -- dump a group of 8 bytes
    let outString = outF (DVU.slice idx 8 bytes)
    in  (formatLinePrefix (DVU.slice idx 8 bytes) (mkPlainAddress addr) outString)
        >< fmtByteGroup bytes (addr + 8) (idx + 8) outF

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

showDisp :: Z80disp
         -> T.Text
showDisp disp
  | disp < 0  = T.pack . show $ disp
  | otherwise = T.append "+" (T.pack . show $ disp)
