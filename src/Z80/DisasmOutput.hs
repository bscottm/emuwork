module Z80.DisasmOutput
  ( z80AnalyticDisassembly
  , z80AnalyticDisassemblyOutput
  ) where

-- import Debug.Trace

import System.IO
import Data.Char
import Data.Tuple
import Control.Lens
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
  dstate ^. symbolTab ^& formatSymTab $ formattedDisSeq dstate
  where
    formattedDisSeq z80dstate = (fixupSymbols z80dstate) ^. disasmSeq ^& Foldable.foldl formatElem Seq.empty
    formatElem accSeq (DisasmInsn addr bytes addrLabel ins cmnt) =
      accSeq >< formatLinePrefix bytes addr addrLabel (formatIns ins cmnt dstate)
    formatElem accSeq pseudo                                     =
      accSeq >< formatPseudo pseudo

-- | Generate the "analytic" version of the output (opcodes, ASCII representation) and output to an 'IO' handle.
z80AnalyticDisassemblyOutput :: Handle
                             -> Z80disassembly
                             -> IO ()
z80AnalyticDisassemblyOutput hOut dstate = Foldable.traverse_ (TIO.hPutStrLn hOut) $ z80AnalyticDisassembly dstate

-- | Pass over the disassembly sequence, translating absolute addresses into symbolic addresses.
fixupSymbols :: Z80disassembly
             -> Z80disassembly
fixupSymbols z80dstate = disasmSeq %~ (fmap (xlatSymbol (z80dstate ^. symbolTab))) $ z80dstate
  where
    xlatSymbol symtab (DisasmInsn addr opcodes _addrLabel insn cmnt) =
      DisasmInsn addr opcodes (xlatAddrLabel symtab addr) (xlatInsnAddr symtab insn) cmnt
    xlatSymbol symtab (ByteRange addr _addrLabel bytes)              = ByteRange addr (xlatAddrLabel symtab addr) bytes
    xlatSymbol symtab (Addr addr _addrLabel theAddr bytes)           = Addr addr (xlatAddrLabel symtab addr) theAddr bytes
    xlatSymbol symtab (AsciiZ addr _addrLabel asciiz)                = AsciiZ addr (xlatAddrLabel symtab addr) asciiz
    xlatSymbol symtab (Ascii addr _addrLabel ascii)                  = Ascii addr (xlatAddrLabel symtab addr) ascii
    xlatSymbol _symtab elt                                           = elt

    -- Translate addresses into labels
    xlatAddrLabel symtab addr =
      case addr `Map.lookup` symtab of
        Just sym   -> sym
        Nothing    -> T.empty

    xlatInsnAddr symtab insn = 
      let symName addr
            | (AbsAddr absAddr) <- addr =
                case absAddr `Map.lookup` symtab of
                  Just sym -> SymAddr sym
                  Nothing  -> addr
            | otherwise                  = addr
      in  case insn of
            LD (HLIndirectStore addr)    -> LD (HLIndirectStore (symName addr))
            LD (HLIndirectLoad addr)     -> LD (HLIndirectLoad (symName addr))
            LD (RPIndirectLoad rp addr)  -> LD (RPIndirectLoad rp (symName addr))
            LD (RPIndirectStore rp addr) -> LD (RPIndirectStore rp (symName addr))
            DJNZ addr                    -> DJNZ (symName addr)
            JR addr                      -> JR (symName addr)
            JRCC cc addr                 -> JRCC cc (symName addr)
            JP addr                      -> JP (symName addr)
            JPCC cc addr                 -> JPCC cc (symName addr)
            CALL addr                    -> CALL (symName addr)
            CALLCC cc addr               -> CALLCC cc (symName addr)
            _otherwise                   -> insn

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
          -> Z80disassembly             -- ^ Disassembler state
          -> T.Text                     -- ^ Formatted result
formatIns ins cmnt dstate = let (mnemonic, opers) = formatInstruction dstate ins
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
                 -> Z80addr                     -- ^ Address of this output line
                 -> T.Text                      -- ^ Address label
                 -> T.Text                      -- ^ Output to emit (formatted instruction, ...)
                 -> Seq T.Text                  -- ^ Resulting 'T.Text' output sequence
formatLinePrefix bytes addr addrLabel outString =
  let label               = if (not . T.null) addrLabel then
                              addrLabel `T.snoc` ':'
                            else
                              addrLabel
      bytesToChars vec    = padTo lenAsChars $ DVU.foldl (\s x -> T.append s (mkPrintable x)) T.empty vec
      mkPrintable x       = if x > 0x20 && x < 0x7f then (T.singleton . chr . fromIntegral) x; else " "
      linePrefix          = T.concat [ upperHex addr
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
        ( Seq.singleton $ T.concat [ upperHex addr
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
formatInstruction :: Z80disassembly             -- ^ Disassembly state, used to grab the disassembly symbol table
                  -> Z80instruction             -- ^ Instruction to format
                  -> (T.Text, T.Text)           -- ^ '(mnemonic, operands)' result tuple

formatInstruction _dstate (Z80undef _)            = zeroOperands "???"
formatInstruction _dstate (LD x)                  = oneOperand "LD" x
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
formatInstruction _dstate (EXC AFAF')             = ("EX", "AF, AF'")
formatInstruction _dstate (EXC SPHL)              = ("EX", "(SP), HL")
formatInstruction _dstate (EXC DEHL)              = ("EX", "DE, HL")
formatInstruction _dstate DI                      = zeroOperands "DI"
formatInstruction _dstate EI                      = zeroOperands "EI"
formatInstruction _dstate (EXC Primes)            = zeroOperands "EXX"
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
formatInstruction _dstate (RST rst)               = ("RST", (upperHex rst))

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
  formatOperand (HLIndLoad r)             = T.append (formatOperand r) ", (HL)"
  formatOperand (IXIndLoad r disp)        = T.append (formatOperand r) (T.append ", (IX" (T.append (showDisp disp) ")"))
  formatOperand (IYIndLoad r disp)        = T.append (formatOperand r) (T.append ", (IY" (T.append (showDisp disp) ")"))
  formatOperand AccBCIndirect             = "A, (BC)"
  formatOperand AccDEIndirect             = "A, (DE)"
  formatOperand (AccImm16Indirect addr)   = T.append "A, " (formatOperand addr)
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

formatPseudo (ByteRange sAddr addrLabel bytes) = 
  let initSlice     = DVU.slice 0 (if DVU.length bytes <= 8; then DVU.length bytes; else 8) bytes
      mkBytes vec   =  T.concat [ padTo lenMnemonic "DB"
                                , T.intercalate ", " [ oldStyleHex x | x <- DVU.toList vec ]
                                ]
  in  formatLinePrefix initSlice sAddr addrLabel (mkBytes initSlice)
      >< fmtByteGroup bytes (sAddr + 8) 8 mkBytes

formatPseudo (ExtPseudo (ByteExpression addr expr word)) = 
  let outF _vec = T.concat [ padTo lenMnemonic "DB"
                              , if (not . T.null) expr then
                                  expr
                                else
                                  upperHex word
                            ]
  in  fmtByteGroup (DVU.singleton word) addr 0 outF

formatPseudo (Addr sAddr sAddrLabel addr bytes) =
  formatLinePrefix bytes sAddr sAddrLabel (T.append (padTo lenMnemonic "DA") (formatOperand addr))

formatPseudo (AsciiZ sAddr addrLabel str) =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      nonNullSlice  = DVU.slice 0 (DVU.length str - 1) str
      mkString      = T.cons '\'' (T.snoc (T.pack [ (chr . fromIntegral) x | x <- DVU.toList nonNullSlice ]) '\'')
      outF _vec     = T.empty
  in  formatLinePrefix initSlice sAddr addrLabel (T.append (padTo lenMnemonic "DZ") mkString)
      >< fmtByteGroup str (sAddr + 8) 8 outF

formatPseudo (Ascii sAddr addrLabel str) =
  let initSlice     = DVU.slice 0 (if DVU.length str <= 8; then DVU.length str; else 8) str
      mkString      = T.cons '\'' (T.snoc (T.pack [ (chr . fromIntegral) x | x <- DVU.toList str ]) '\'')
      outF _vec     = T.empty
  in  formatLinePrefix initSlice sAddr addrLabel (T.append (padTo lenMnemonic "DS") mkString)
      >< fmtByteGroup str (sAddr + 8) 8 outF

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
    in  formatLinePrefix (DVU.slice idx (DVU.length bytes - idx) bytes) addr T.empty outString
  | otherwise =
    -- dump a group of 8 bytes
    let outString = outF (DVU.slice idx 8 bytes)
    in  (formatLinePrefix (DVU.slice idx 8 bytes) addr T.empty outString)
        >< fmtByteGroup bytes (addr + 8) (idx + 8) outF

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

showDisp :: Z80disp
         -> T.Text
showDisp disp
  | disp < 0  = T.pack . show $ disp
  | otherwise = T.append "+" (T.pack . show $ disp)
