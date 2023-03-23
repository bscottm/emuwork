{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

import           Prelude               hiding (seq)
import           Lens.Micro.Platform  ((^.), view)
import           Data.Char             (chr)
import qualified Data.Foldable         as Foldable
import           Data.Generics.Aliases (mkT)
import           Data.Generics.Schemes (everywhere)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as H
import           Data.List             (sortBy)
import           Data.Sequence         (Seq, (<|), (><), (|>))
import qualified Data.Sequence         as Seq
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Tuple            ()
import           Data.Vector.Unboxed   (Vector)
import qualified Data.Vector.Unboxed   as DVU
import Generics.SOP
    ( Proxy(..),
      Generic(from, Code),
      HCollapse(hcollapse),
      All2,
      mapIK,
      hcmap )
import System.IO                       (Handle)

import Machine
    ( disEltAddress,
      disEltLabel,
      disasmSymbolTable,
      mkPlainAddress,
      makeUpper,
      padTo,
      textSpace,
      DisElement(LineComment, DisasmInsn, ByteRange, ExtPseudo, Addr,
                 AsciiZ, Ascii, DisOrigin, Equate),
      DisEltAddress(..),
      SymAbsAddr(..),
      ShowHex(asHex) )
import Z80.Disassembler
    ( Z80PseudoOps(ByteExpression),
      Z80DisasmElt,
      Z80disassembly,
      z80AddrInDisasmRange )
-- Minor conflicts with Generics.SOP... <sigh!>
import           Z80.InstructionSet hiding (POP, Z)
import qualified Z80.InstructionSet as Z80
import Z80.Processor (Z80addr, Z80word)

-- | Format the "analytic" version of the disassembled Z80 sequence as a 'Text'
-- sequence. This outputs the address, opcode bytes, the opcodes as ASCII,
-- followed by the standard \'label: instruction ; comment\' output.
z80AnalyticDisassembly :: Z80disassembly
                       -> Seq Z80DisasmElt
                       -> Seq T.Text
z80AnalyticDisassembly dstate disasmSeq =
  Foldable.foldr formatElt Seq.empty $ everywhere (mkT fixupSymbol . mkT fixupEltAddress) disasmSeq
  where
    symtab = dstate ^. disasmSymbolTable
    -- Translate an absolute address, generally hidden inside an instruction operand, into a symbolic address
    -- if present in the symbol table.
    fixupSymbol addr@(AbsAddr absAddr) = maybe addr SymAddr (absAddr `H.lookup` symtab)
    fixupSymbol other                  = other

    -- Lookup address labels in the symbol table
    fixupEltAddress disAddr@(Plain absAddr) = maybe disAddr (Labeled absAddr) (absAddr `H.lookup` symtab)
    fixupEltAddress other                   = other

    formatElt (DisasmInsn addr insVec insn cmnt) seq = formatLinePrefix insVec addr (fmtWithComment fmtInstruction cmnt') >< seq
      where
        fmtInstruction = T.concat $ [padTo lenMnemonic . insMnemonic, z80FormatOperands] <*> [insn]

        fmtWithComment inp insComment
          | T.null insComment
          = inp
          | otherwise
          = T.concat [padTo lenInstruction inp, "; ", insComment]

        -- For 16-bit constant loads, point out potential internal references
        cmnt'
          | LD (RPair16ImmLoad _rp (AbsAddr addr')) <- insn
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

    formatElt pseudo seq = formatPseudo pseudo >< seq

    
-- | Generate the "analytic" version of the output (opcodes, ASCII representation) and output to an 'IO' handle.
z80AnalyticDisassemblyOutput :: Handle
                             -> Z80disassembly
                             -> Seq Z80DisasmElt
                             -> IO ()
z80AnalyticDisassemblyOutput hOut dstate disasmSeq =
  Foldable.traverse_ (TIO.hPutStrLn hOut) $ z80AnalyticDisassembly dstate disasmSeq

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
    _        -> gFormatOperands insn


-- | Generic formatting traversal over the Z80 instruction operands
gFormatOperands ::(Generic x, All2 Z80operand (Code x))
                 => x
                 -> T.Text
gFormatOperands {-elt-} =
  T.intercalate ", " . hcollapse . hcmap disOperandProxy (mapIK formatOperand) . from {-elt-}
  where
    disOperandProxy = Proxy :: Proxy Z80operand

-- | Format the accumulated symbol table as a sequence of 'T.Text's, in columnar format
formatSymTab :: HashMap Z80addr T.Text
             -> Seq T.Text
formatSymTab symTab =
  let !maxsym     = H.foldr (\str len -> max len (T.length str)) 0 symTab
      !totalCols  = fromIntegral(((lenOutputLine - maxsym) `div` (maxsym + extraSymPad)) + 1) :: Int
      !symsAsList = H.toList symTab
      byNameSyms  = sortBy compareByName symsAsList
      byAddrSyms  = sortBy compareByAddr symsAsList
      byAddrSeq   = T.empty
                    <| T.empty
                    <| "Symbol Table (numeric):"
                    <| T.empty
                    <| columnar (Foldable.foldr formatSymbol Seq.empty byAddrSyms)
      byNameSeq   = T.empty
                    <| T.empty
                    <| "Symbol Table (alpha):"
                    <| T.empty
                    <| columnar (Foldable.foldr formatSymbol Seq.empty byNameSyms)
      -- Consolidate sequence into columnar format
      columnar symSeq = if Seq.length symSeq >= totalCols then
                          let (thisCol, rest) = Seq.splitAt totalCols symSeq
                          in  T.intercalate "  " (Foldable.toList thisCol) <| columnar rest
                        else
                          Seq.singleton $ T.intercalate "  " $ Foldable.toList symSeq

      -- Extra symbol padding: 4 for the hex address, 3 for " = " and 2 for intercolumn spacing
      extraSymPad = 4 + 3 + 2

      -- Format the symbol:
      formatSymbol (addr, sym) theSeq = T.concat [ padTo maxsym sym , " = " , upperHex addr ] <| theSeq
      -- Comparison functions
      compareByName (_, n1) (_, n2) = compare n1 n2
      compareByAddr (a1, _) (a2, _) = compare a1 a2

    in  byNameSeq >< byAddrSeq

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
      mkPrintable x       = if x > 0x20 && x < 0x7f
                              then (chr . fromIntegral) x
                              else ' '
      linePrefix          = T.concat [ upperHex (disEltAddress addr)
                                      , ": "
                                      , padTo lenInsBytes . T.intercalate " " $ [ upperHex x | x <- DVU.toList bytes ]
                                      , "|"
                                      , padTo lenAsChars . T.pack . map mkPrintable $ DVU.toList bytes
                                      , "| "
                                      ]
  in  if T.length label < (lenSymLabel - 2) then
        Seq.singleton (T.concat [ linePrefix
                                , padTo lenSymLabel label
                                , outString
                                ])
      else
        Seq.singleton (T.concat [ upperHex (disEltAddress addr)
                                , ": "
                                , T.replicate lenInsBytes textSpace
                                , "|"
                                , T.replicate lenAsChars textSpace
                                , "| "
                                , label
                                ])
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
insMnemonic LD{}              = "LD"
insMnemonic INC{}             = "INC"
insMnemonic DEC{}             = "DEC"
insMnemonic INC16{}           = "INC"
insMnemonic DEC16{}           = "DEC"
insMnemonic ADD8{}            = "ADD"
insMnemonic ADD16{}           = "ADD"
insMnemonic ADC8{}            = "ADC"
insMnemonic ADC16{}           = "ADC"
insMnemonic SUB{}             = "SUB"
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

-- instance Z80operand Int8 where
--  formatOperand = T.pack . show

instance Z80operand OperLD where
  formatOperand (Reg8Reg8 r r')           = T.intercalate ", " ([formatOperand] <*> [r, r'])
  formatOperand (Reg8Imm r imm)           = T.intercalate ", " [formatOperand r, formatOperand imm]
  formatOperand AccBCIndirect             = "A, (BC)"
  formatOperand AccDEIndirect             = "A, (DE)"
  formatOperand (AccImm16Indirect addr)   = T.concat [ "A, (", formatOperand addr , ")" ]
  formatOperand AccIReg                   = "A, I"
  formatOperand AccRReg                   = "A, R"
  formatOperand BCIndirectStore           = "(BC), A"
  formatOperand DEIndirectStore           = "(DE), A"
  formatOperand (Imm16IndirectStore addr) = T.concat [ "(", formatOperand addr, "), A"]
  formatOperand IRegAcc                   = "I, A"
  formatOperand RRegAcc                   = "R, A"
  formatOperand (RPair16ImmLoad rp imm)   = T.intercalate ", " [formatOperand rp, formatOperand imm]
  formatOperand (RPIndirectLoad rp addr)  = T.concat [ formatOperand rp , ", (" , formatOperand addr , ")" ]
  formatOperand (RPIndirectStore rp addr) = T.concat [ "(" , formatOperand addr , "), " , formatOperand rp ]

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

formatPseudo (Equate label addr) = Seq.singleton $ T.concat [ T.replicate lenOutputPrefix textSpace
                                                            , padTo lenSymLabel label
                                                            , padTo lenMnemonic "="
                                                            , oldStyleHex addr
                                                            ]

formatPseudo (LineComment comment) = Foldable.foldr (\cmnt acc -> T.concat [ T.replicate lenOutputPrefix textSpace
                                                                           , "; "
                                                                           , cmnt
                                                                           ] <| acc)
                                                    Seq.empty
                                                    (T.lines comment)

formatPseudo _unknownPseudo = Seq.singleton "[!!Unknown pseudo instruction]"

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
