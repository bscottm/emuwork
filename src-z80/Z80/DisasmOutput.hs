{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Z80 disassmbler output.
--
-- Disassembler output consists of two elements: the "analytic" part (address, opcode bytes and ASCII) and the "assembler" part.
-- The assembler part is Misosys EDAS-compatible, so, if the "analytic" part is stripped from the output, the resulting "assembler"
-- part could be fed into the Misosys-EDAS compatible assembler to regenerate the object code.
module Z80.DisasmOutput
  ( z80AnalyticDisassembly
  , z80AnalyticDisassemblyOutput
  , z80FormatSymbolTable
  , z80ShortInsnFormat
  , Z80operand(..)
  ) where

-- import Debug.Trace

import           Data.Char                 (chr)
import           Data.Function             (on)
import           Data.Generics.Aliases     (mkT)
import           Data.Generics.Schemes     (everywhere)
import qualified Data.HashMap.Strict as H  (HashMap, lookup, toList, elems)
import           Data.List                 (sortBy)
import qualified Data.List.Split as DLS    (chunksOf)
import           Data.Sequence             (Seq, (><), (|>))
import qualified Data.Sequence             as Seq
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Tuple                ()
import           Data.Vector.Unboxed       (Vector)
import qualified Data.Vector.Unboxed       as DVU

import           Generics.SOP              (All2, Generic (Code, from), HCollapse (hcollapse), Proxy (..), hcmap, mapIK)

import           Lens.Micro.Platform       (view, (^.))

import           Machine.DisassemblerTypes (DisElement (Addr, Ascii, AsciiZ, ByteRange, DisOrigin, DisasmInsn, Equate, ExtPseudo, LineComment),
                                            DisEltAddress (..), disEltAddress, disEltLabel, disasmSymbolTable, mkPlainAddress)
import           Machine.System            (SymAbsAddr (..))
import           Machine.Utils             (ShowHex (asHex), makeUpper, padTo, textSpace)

import           Prelude                   hiding (seq)

import           System.IO                 (Handle)

import           Z80.Disassembler          (Z80DisasmElt, Z80PseudoOps (ByteExpression), Z80disassembly, z80AddrInDisasmRange)
import qualified Z80.InstructionSet        as Z80
import           Z80.InstructionSet        hiding (POP, Z)
import           Z80.Processor             (Z80addr, Z80word)

-- | Format the "analytic" version of the disassembled Z80 sequence as a 'Text'
-- sequence. This outputs the address, opcode bytes, the opcodes as ASCII,
-- followed by the standard \'label: instruction ; comment\' output.
z80AnalyticDisassembly :: Z80disassembly
                       -> Seq Z80DisasmElt
                       -> Seq T.Text
z80AnalyticDisassembly dstate {-disasmSeq-} =
  foldMap (formatElt . everywhere (mkT fixupSymbol . mkT fixupEltAddress)) {-disasmSeq-}
  where
    symtab = dstate ^. disasmSymbolTable
    -- Translate an absolute address, generally hidden inside an instruction operand, into a symbolic address
    -- if present in the symbol table.
    fixupSymbol addr = case addr of
      AbsAddr absAddr -> maybe addr SymAddr (absAddr `H.lookup` symtab)
      _               -> addr

    -- Lookup address labels in the symbol table
    fixupEltAddress disAddr = case disAddr of
      Plain absAddr -> maybe disAddr (Labeled absAddr) (absAddr `H.lookup` symtab)
      _             -> disAddr

    formatElt (DisasmInsn addr insVec insn cmnt) =
      formatLinePrefix insVec addr (fmtWithComment fmtInstruction cmnt')
      where
        fmtInstruction = T.concat $ [padTo lenMnemonic . insMnemonic, z80FormatOperands] <*> [insn]

        fmtWithComment inp insComment =
          if T.null insComment
          then inp
          else T.concat [padTo lenInstruction inp, "; ", insComment]

        -- For 16-bit constant loads, point out potential internal references
        cmnt'
          | LDr16imm (Reg16Imm _rp (AbsAddr addr')) <- insn
          -- 0x0 tends to be a constant, so it's likely not an internal reference.
          , z80AddrInDisasmRange addr' dstate && 0 < addr'
          = appendIntRef cmnt
          | otherwise
          = cmnt

        intRefMsg = "poss. internal ref"
        appendIntRef disCmnt
          | T.null disCmnt
          = intRefMsg
          | otherwise
          = T.intercalate " " [disCmnt, intRefMsg]

    formatElt pseudo = formatPseudo pseudo
    
-- | Generate the "analytic" version of the output (opcodes, ASCII representation) and output to an 'IO' handle.
z80AnalyticDisassemblyOutput :: Handle
                             -> Z80disassembly
                             -> Seq Z80DisasmElt
                             -> IO ()
z80AnalyticDisassemblyOutput hOut dstate disasmSeq =
  TIO.hPutStr hOut $ foldMap (<> "\n") $ z80AnalyticDisassembly dstate disasmSeq

-- | Format the disassembler's symbol table.
z80FormatSymbolTable :: Z80disassembly
                     -> Seq T.Text
z80FormatSymbolTable {-dstate-} = formatSymTab <$> view disasmSymbolTable {-dstate-}


-- | Short instruction output formatter, e.g., "INC BC"
z80ShortInsnFormat
  :: Z80instruction
  -> T.Text
z80ShortInsnFormat ins = T.intercalate " " [insMnemonic ins, z80FormatOperands ins]

-- Catch some of the special cases before calling the generic operand formatter.
z80FormatOperands
  :: Z80instruction
  -> T.Text
z80FormatOperands insn =
  case insn of
    IN port ->
      case port of
        PortImm imm -> formatOperand imm
        CIndIO reg8 -> T.append (formatOperand reg8) ", (C)"
        CIndIO0     -> "(C)"
    OUT port ->
      case port of
        PortImm imm -> formatOperand imm
        CIndIO reg8 -> T.append "(C), " (formatOperand reg8)
        CIndIO0     -> "(C), 0"
    RST rst  -> asHex rst
    JPHL     -> "(HL)"
    JPIX     -> "(IX)"
    JPIY     -> "(IY)"
    LDSPHL   -> "SP, HL"
    LDSPIX   -> "SP, IX"
    LDSPIY   -> "SP, IY"
    -- Weird SUB mnemonic asymmetry... it's all "ADD A, _", "ADC A, _"
    -- and "SBC A, _", but SUB doesn't follow the pattern.
    SUB8 (ALUAcc src) -> formatOperand src
    _        -> gFormatOperands insn


-- | Generic formatting traversal over the Z80 instruction operands
gFormatOperands ::(Generic  x, All2 Z80operand (Code x))
                 => x
                 -> T.Text
gFormatOperands {-elt-} =
  T.intercalate ", " . hcollapse . hcmap disOperandProxy (mapIK formatOperand) . from {-elt-}
  where
    disOperandProxy = Proxy :: Proxy Z80operand

-- | Format the accumulated symbol table in columnar format, sorted by symbols name and
-- then formatted by address.
formatSymTab :: H.HashMap Z80addr T.Text
             -> Seq T.Text
formatSymTab symTab =
  let maxSymLen             = maximum (T.length <$> H.elems symTab)
      totalCols             = 1+ ((lenOutputLine - maxSymLen) `div` (maxSymLen + extraSymPad))
      symsAsList            = H.toList symTab
      formatSym (addr, sym) = padTo maxSymLen sym <> " = " <> upperHex addr
      symColumns {-syms-}   = DLS.chunksOf totalCols . map formatSym {-syms-}
      symLines {-syms-}     = T.intercalate "\n" . map (T.intercalate "   ") . symColumns {-sym-}
      byNameAddr            = ["Symbol Table (alpha):", "", symLines (sortBy (compare `on` snd) symsAsList)]
      byNameSeq             = ["Symbol Table (numeric):", "", symLines (sortBy (compare `on` fst) symsAsList)]
      -- Extra symbol padding: 4 for the hex address, 3 for " = " and 2 for intercolumn spacing
      extraSymPad           = 4 + 3 + 2
    in  Seq.fromList (["", ""] ++ byNameAddr ++ ["", ""] ++ byNameSeq)

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
  let addrLabel       = disEltLabel addr
      label           = if T.null addrLabel then "" else addrLabel `T.snoc` ':'
      printable x     = if x > 0x20 && x < 0x7f then chr (fromIntegral x) else ' '
      linePrefix      = T.concat [ upperHex (disEltAddress addr)
                                  , ": "
                                  , padTo lenInsBytes . T.intercalate " " $ [ upperHex x | x <- DVU.toList bytes ]
                                  , "|"
                                  , padTo lenAsChars . T.pack . map printable $ DVU.toList bytes
                                  , "| "
                                  ]
      formattedPrefix = Seq.singleton $ T.concat [ linePrefix, padTo lenSymLabel label, outString ]
      truncatedPrefix = Seq.singleton $ T.concat [ upperHex (disEltAddress addr)
                                 , ": "
                                 , T.replicate lenInsBytes textSpace
                                 , "|"
                                 , T.replicate lenAsChars textSpace
                                 , "| "
                                 , label
                                 ]
  in  if T.length label < (lenSymLabel - 2)
      then formattedPrefix
      else truncatedPrefix
            |> T.concat [ linePrefix
                        , T.replicate lenSymLabel textSpace
                        , outString
                        ]

-- Lengths of various output columns:

-- | Length of instruction opcode output: 8 bytes max to dump, 3 spaces per byte
lenInsBytes :: Int
lenInsBytes = lenAsChars * 3
-- | Length of opcode-as-characters output: 8 characters, as the bytes are dumped as characters
lenAsChars :: Int
lenAsChars = 8
-- | Length of the label colum: 8 characters plus ": "
lenSymLabel :: Int
lenSymLabel = T.length "XXXXXXXXXXXX: "
-- | Total length of the instruction+operands output
lenInstruction :: Int
lenInstruction = 24
-- | Length of the instruction mnemonic
lenMnemonic :: Int
lenMnemonic = 6
-- | Length of the output address
lenAddress :: Int
lenAddress = T.length "AAAA: "
-- | Length of the line's prefix
lenOutputPrefix :: Int
lenOutputPrefix = lenAddress + lenInsBytes + lenAsChars + 3
-- | Total length of each output line:
lenOutputLine :: Int
lenOutputLine = lenOutputPrefix + lenSymLabel + lenInstruction

insMnemonic :: Z80instruction -> T.Text
insMnemonic Z80undef{}        = "???"
insMnemonic LDAspecial{}      = "LD"
insMnemonic LDr8r8{}          = "LD"
insMnemonic LDr8imm{}         = "LD"
insMnemonic LDAmem{}          = "LD"
insMnemonic LDr16mem{}        = "LD"
insMnemonic LDr16imm{}        = "LD"
insMnemonic INC{}             = "INC"
insMnemonic DEC{}             = "DEC"
insMnemonic INC16{}           = "INC"
insMnemonic DEC16{}           = "DEC"
insMnemonic ADD8{}            = "ADD"
insMnemonic ADD16{}           = "ADD"
insMnemonic ADC8{}            = "ADC"
insMnemonic ADC16{}           = "ADC"
insMnemonic SUB8{}            = "SUB"
insMnemonic SBC8{}            = "SBC"
insMnemonic SBC16{}           = "SBC"
insMnemonic AND{}             = "AND"
insMnemonic XOR{}             = "XOR"
insMnemonic OR{}              = "OR"
insMnemonic CP{}              = "CP"
insMnemonic HALT              = "HALT"
insMnemonic NOP               = "NOP"
insMnemonic (EXC Primes)      = "EXX"
insMnemonic (EXC _)           = "EX"
insMnemonic DI                = "DI"
insMnemonic EI                = "EI"
insMnemonic JPHL              = "JP"
insMnemonic JPIX              = "JP"
insMnemonic JPIY              = "JP"
insMnemonic LDSPHL            = "LD"
insMnemonic LDSPIX            = "LD"
insMnemonic LDSPIY            = "LD"
insMnemonic RLCA              = "RLCA"
insMnemonic RRCA              = "RRCA"
insMnemonic RLA               = "RLA"
insMnemonic RRA               = "RRA"
insMnemonic DAA               = "DAA"
insMnemonic CPL               = "CPL"
insMnemonic SCF               = "SCF"
insMnemonic CCF               = "CCF"
insMnemonic DJNZ{}            = "DJNZ"
insMnemonic JR{}              = "JR"
insMnemonic JRCC{}            = "JR"
insMnemonic JP{}              = "JP"
insMnemonic JPCC{}            = "JP"
insMnemonic IN{}              = "IN"
insMnemonic OUT{}             = "OUT"
insMnemonic CALL{}            = "CALL"
insMnemonic CALLCC{}          = "CALL"
insMnemonic RET               = "RET"
insMnemonic RETCC{}           = "RET"
insMnemonic PUSH{}            = "PUSH"
insMnemonic (Z80.POP _)       = "POP"
insMnemonic RST{}             = "RST"
insMnemonic RLC{}             = "RLC"
insMnemonic RRC{}             = "RRC"
insMnemonic RL{}              = "RL"
insMnemonic RR{}              = "RR"
insMnemonic SLA{}             = "SLA"
insMnemonic SRA{}             = "SRA"
insMnemonic SLL{}             = "SLL"
insMnemonic SRL{}             = "SRL"
insMnemonic BIT{}             = "BIT"
insMnemonic RES{}             = "RES"
insMnemonic SET{}             = "SET"
insMnemonic NEG               = "NEG"
insMnemonic RETI              = "RETI"
insMnemonic RETN              = "RETN"
insMnemonic IM{}              = "IM"
insMnemonic RLD               = "RLD"
insMnemonic RRD               = "RRD"
insMnemonic LDI               = "LDI"
insMnemonic CPI               = "CPI"
insMnemonic INI               = "INI"
insMnemonic OUTI              = "OUTI"
insMnemonic LDD               = "LDD"
insMnemonic CPD               = "CPD"
insMnemonic IND               = "IND"
insMnemonic OUTD              = "OUTD"
insMnemonic LDIR              = "LDIR"
insMnemonic CPIR              = "CPIR"
insMnemonic INIR              = "INIR"
insMnemonic OTIR              = "OTIR"
insMnemonic LDDR              = "LDDR"
insMnemonic CPDR              = "CPDR"
insMnemonic INDR              = "INDR"
insMnemonic OTDR              = "OTDR"
-- The undocumented CB prefixed instructions
insMnemonic RLCidx{}          = "RLC"
insMnemonic RRCidx{}          = "RRC"
insMnemonic RLidx{}           = "RL"
insMnemonic RRidx{}           = "RR"
insMnemonic SLAidx{}          = "SLA"
insMnemonic SRAidx{}          = "SRA"
insMnemonic SLLidx{}          = "SLL"
insMnemonic SRLidx{}          = "SRL"
insMnemonic RESidx{}          = "RES"
insMnemonic SETidx{}          = "SET"

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassembler operand format function class. Make life easy on ourselves when
-- formatting assembler operands.
class Z80operand x where
  -- | Convert an operand into its appropriate representation
  formatOperand :: x -> T.Text

instance (Z80operand a)
         => Z80operand [a] where
  formatOperand = T.concat . map formatOperand

instance Z80operand Z80word where
  formatOperand = oldStyleHex

instance Z80operand Z80addr where
  formatOperand = oldStyleHex

instance Z80operand Reg8Reg8 where
  formatOperand (Reg8Reg8 r r')           = T.intercalate ", " (formatOperand <$> [r, r'])

instance Z80operand Reg8Imm where
  formatOperand (Reg8Imm r imm)           = T.intercalate ", " [formatOperand r, formatOperand imm]

instance Z80operand AMemXfer where
  formatOperand FromBCindirect            = "A, (BC)"
  formatOperand FromDEindirect            = "A, (DE)"
  formatOperand ToBCindirect              = "(BC), A"
  formatOperand ToDEindirect              = "(DE), A"
  formatOperand (AccFromMem addr)         = T.concat [ "A, (", formatOperand addr , ")" ]
  formatOperand (AccToMem addr)           = T.concat [ "(", formatOperand addr, "), A" ]

instance Z80operand Reg16Mem where
  formatOperand (ToReg16 rp addr)         = T.concat [ formatOperand rp , ", (" , formatOperand addr , ")" ]
  formatOperand (FromReg16 rp addr)       = T.concat [ "(" , formatOperand addr , "), " , formatOperand rp ]

instance Z80operand Reg16Imm where
  formatOperand (Reg16Imm rp imm)         = T.intercalate ", " [formatOperand rp, formatOperand imm]

instance Z80operand AccumSpecials where
  formatOperand FromItoA                   = "A, I"
  formatOperand FromRtoA                   = "A, R"
  formatOperand FromAtoI                   = "I, A"
  formatOperand FromAtoR                   = "R, A"

instance Z80operand OperALU where
  formatOperand (ALUimm imm)  = formatOperand imm
  formatOperand (ALUreg8 r)   = formatOperand r

instance Z80operand DestALUAcc where
  formatOperand (ALUAcc opnd) = T.append "A, " $ gFormatOperands opnd

instance Z80operand DestALU16 where
  formatOperand DestHL = "HL"
  formatOperand DestIX = "IX"
  formatOperand DestIY = "IY"

instance Z80operand RegPairSP where
  formatOperand (RPair16 r) = formatOperand r
  formatOperand SP          = "SP"

instance Z80operand Z80reg8 where
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

instance Z80operand Z80reg16 where
  formatOperand BC = "BC"
  formatOperand DE = "DE"
  formatOperand HL = "HL"
  formatOperand IX = "IX"
  formatOperand IY = "IY"

instance Z80operand Z80condC where
  formatOperand NZ    = "NZ"
  formatOperand Z80.Z = "Z"
  formatOperand NC    = "NC"
  formatOperand CY    = "C"
  formatOperand PO    = "PO"
  formatOperand PE    = "PE"
  formatOperand POS   = "P"
  formatOperand MI    = "M"

instance Z80operand RegPairAF where
  formatOperand (AFPair16 r) = formatOperand r
  formatOperand AF           = "AF"

instance (Z80operand addrType) => Z80operand (SymAbsAddr addrType) where
  formatOperand (AbsAddr addr)  = formatOperand addr
  formatOperand (SymAddr label) = label

instance Z80operand Z80ExchangeOper where
  formatOperand AFAF'  = "AF, AF'"
  formatOperand DEHL   = "DE, HL"
  formatOperand SPHL   = "(SP), HL"
  formatOperand SPIX   = "(SP), IX"
  formatOperand SPIY   = "(SP), IY"
  formatOperand Primes = T.empty

instance Z80operand OperIO where
  -- Never used, but has to be here for Generics.SOP completeness.
  formatOperand _ = T.empty

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

formatPseudo :: Z80DisasmElt
             -> Seq T.Text

formatPseudo (ByteRange sAddr bytes) =
  let initSlice     = DVU.slice 0 (if DVU.length bytes <= 8; then DVU.length bytes; else 8) bytes
      mkBytes vec   =  T.concat [ padTo lenMnemonic "DB"
                                , T.intercalate ", " [ oldStyleHex x | x <- DVU.toList vec ]
                                ]
  in  formatLinePrefix initSlice sAddr (mkBytes initSlice)
      >< fmtByteGroup (DVU.drop 8 bytes) (disEltAddress sAddr + 8) mkBytes

formatPseudo (ExtPseudo (ByteExpression addr expr word)) =
  let outF _vec = T.concat [ padTo lenMnemonic "DB"
                              , if (not . T.null) expr then
                                  expr
                                else
                                  upperHex word
                            ]
  in  fmtByteGroup (DVU.singleton word) addr outF

formatPseudo (Addr sAddr addr bytes) =
  formatLinePrefix bytes sAddr (T.append (padTo lenMnemonic "DA") (formatOperand addr))

formatPseudo (AsciiZ sAddr str) =
  let initSlice     = DVU.slice 0 (min (DVU.length str) 8) str
      nonNullSlice  = DVU.slice 0 (DVU.length str) str
      mkString      = T.cons '\'' (T.snoc (T.pack [ (chr . fromIntegral) x | x <- DVU.toList nonNullSlice ]) '\'')
      outF _vec     = T.empty
  in  formatLinePrefix initSlice sAddr (T.append (padTo lenMnemonic "DS") mkString)
      >< fmtByteGroup (DVU.drop 8 str) (disEltAddress sAddr + 8) outF

formatPseudo (Ascii sAddr str) =
  let initSlice     = DVU.slice 0 (min (DVU.length str) 8) str
      mkString      = T.cons '\'' (T.snoc (T.pack [ (chr . fromIntegral) x | x <- DVU.toList str ]) '\'')
      outF _vec     = T.empty
  in  formatLinePrefix initSlice sAddr (T.append (padTo lenMnemonic "DS") mkString)
      >< fmtByteGroup (DVU.drop 8 str) (disEltAddress sAddr + 8) outF

formatPseudo (DisOrigin origin) = Seq.singleton $ T.concat [ T.replicate (lenOutputPrefix + lenSymLabel) textSpace
                                                           , padTo lenMnemonic "ORG"
                                                           , asHex origin
                                                           ]

formatPseudo (Equate label addr) = Seq.singleton $ T.concat [ blankPrefix
                                                            , padTo lenSymLabel label
                                                            , padTo lenMnemonic "="
                                                            , oldStyleHex addr
                                                            ]

formatPseudo (LineComment comment) = foldMap (Seq.singleton . (\cmnt ->  T.concat [ blankPrefix, "; ", cmnt ])) (T.lines comment)

formatPseudo _unknownPseudo = Seq.singleton "[!!Unknown pseudo instruction]"

blankPrefix :: T.Text
blankPrefix = T.replicate lenOutputPrefix textSpace

-- | Format groups of bytes by groups of 8
fmtByteGroup :: Vector Z80word
             -> Z80addr
             -> (Vector Z80word -> T.Text)
             -> Seq T.Text
fmtByteGroup bytes addr outF
  | DVU.null bytes
  = Seq.empty
  | otherwise
  = formatLinePrefix chunk (mkPlainAddress addr) outString >< fmtByteGroup (DVU.drop 8 bytes) (addr + 8) outF
    where
      chunk = DVU.take 8 bytes
      outString = outF chunk

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

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
