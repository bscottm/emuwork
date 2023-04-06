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
  -- , Z80operandFormat(..)
  ) where

-- import Debug.Trace

import           Data.Char                     (chr)
import           Data.Function                 (on)
import           Data.Generics.Uniplate.Direct
import qualified Data.HashMap.Strict           as H (HashMap, elems, lookup, toList)
import           Data.List                     (sortBy)
import qualified Data.List.Split               as DLS (chunksOf)
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Sequence                 (Seq, (><), (|>))
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Tuple                    ()
import           Data.Vector.Unboxed           (Vector)
import qualified Data.Vector.Unboxed           as DVU

import           Lens.Micro.Platform           (view, (^.))

import           Machine                       (AbstractAddr (..), DisElement (..), ShowHex (asHex), disasmSymbolTable,
                                                labelAbstractAddr, mkAbstractAddr, padTo, textSpace)

import           Prelude                       hiding (seq)

import           System.IO                     (Handle)

import           Z80.Disassembler              (Z80DisasmElt, Z80PseudoOps (ByteExpression), Z80disassembly,
                                                z80AddrInDisasmRange)
import           Z80.InsnText                  (Z80operandFormat (formatOperand), instructionToText, oldStyleHex, upperHex)
import           Z80.InstructionSet            hiding (POP, Z)
import           Z80.Processor                 (Z80addr, Z80byte)

-- | Format the "analytic" version of the disassembled Z80 sequence as a 'Text'
-- sequence. This outputs the address, opcode bytes, the opcodes as ASCII,
-- followed by the standard \'label: instruction ; comment\' output.
z80AnalyticDisassembly :: Z80disassembly
                       -> Seq Z80DisasmElt
                       -> Seq T.Text
z80AnalyticDisassembly dstate {-disasmSeq-} =
  -- foldMap (formatElt . everywhere (mkT fixupSymbol . mkT fixupEltAddress)) {-disasmSeq-}
  foldMap (formatElt . transformBi fixupSymbol) {-disasmSeq-}
  where
    symtab = dstate ^. disasmSymbolTable
    -- Add labels to AbstractAddr addresses if the address maps to a label in the symbol table.
    fixupSymbol addr = maybe addr (labelAbstractAddr addr) (H.lookup (absAddr addr) symtab)

    formatElt (DisasmInsn addr insVec insn cmnt) =
      formatLinePrefix insVec addr (fmtWithComment fmtInstruction cmnt')
      where
        (mnemonic, opnds) = instructionToText insn
        -- fmtInstruction = T.concat $ [padTo lenMnemonic . insMnemonic, z80FormatOperands] <*> [insn]
        fmtInstruction = T.concat [padTo lenMnemonic mnemonic, opnds]

        fmtWithComment inp insComment
          | T.null insComment
          = inp
          | otherwise
          = T.concat [padTo lenInstruction inp, "; ", insComment]

        -- For 16-bit constant loads, point out potential internal references
        cmnt'
          | LD (Reg16Imm _rp (AbstractAddr addr' label')) <- insn
          -- 0x0 tends to be a constant, so it's likely not an internal reference.
          , 0 < addr' && isNothing label' && z80AddrInDisasmRange addr' dstate
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
z80ShortInsnFormat ins = T.intercalate " " [fst insText, snd insText]
  where
    insText = instructionToText ins

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

-- | Format the beginning of the line (address, bytes, label, etc.)
formatLinePrefix :: Vector Z80byte              -- ^ Opcode vector
                 -> AbstractAddr Z80addr          -- ^ Address of this output line
                 -> T.Text                      -- ^ Output to emit (formatted instruction, ...)
                 -> Seq T.Text                  -- ^ Resulting 'T.Text' output sequence
formatLinePrefix bytes addr outString =
  let addrLabel       = fromMaybe T.empty (absLabel addr)
      label           = if T.null addrLabel then "" else addrLabel `T.snoc` ':'
      printable x     = if x > 0x20 && x < 0x7f then chr (fromIntegral x) else ' '
      linePrefix      = T.concat [ upperHex (absAddr addr)
                                  , ": "
                                  , padTo lenInsBytes . T.intercalate " " $ [ upperHex x | x <- DVU.toList bytes ]
                                  , "|"
                                  , padTo lenAsChars . T.pack . map printable $ DVU.toList bytes
                                  , "| "
                                  ]
      formattedPrefix = Seq.singleton $ T.concat [ linePrefix, padTo lenSymLabel label, outString ]
      truncatedPrefix = Seq.singleton $ T.concat [ upperHex (absAddr addr)
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

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

formatPseudo :: Z80DisasmElt
             -> Seq T.Text

formatPseudo (ByteRange sAddr bytes) =
  let initSlice     = DVU.slice 0 (if DVU.length bytes <= 8; then DVU.length bytes; else 8) bytes
      mkBytes vec   =  T.concat [ padTo lenMnemonic "DB"
                                , T.intercalate ", " [ oldStyleHex x | x <- DVU.toList vec ]
                                ]
  in  formatLinePrefix initSlice sAddr (mkBytes initSlice)
      >< fmtByteGroup (DVU.drop 8 bytes) (absAddr sAddr + 8) mkBytes

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
      >< fmtByteGroup (DVU.drop 8 str) (absAddr sAddr + 8) outF

formatPseudo (Ascii sAddr str) =
  let initSlice     = DVU.slice 0 (min (DVU.length str) 8) str
      mkString      = T.cons '\'' (T.snoc (T.pack [ (chr . fromIntegral) x | x <- DVU.toList str ]) '\'')
      outF _vec     = T.empty
  in  formatLinePrefix initSlice sAddr (T.append (padTo lenMnemonic "DS") mkString)
      >< fmtByteGroup (DVU.drop 8 str) (absAddr sAddr + 8) outF

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
fmtByteGroup :: Vector Z80byte
             -> Z80addr
             -> (Vector Z80byte -> T.Text)
             -> Seq T.Text
fmtByteGroup bytes addr outF
  | DVU.null bytes
  = Seq.empty
  | otherwise
  = formatLinePrefix chunk (mkAbstractAddr addr) outString >< fmtByteGroup (DVU.drop 8 bytes) (addr + 8) outF
    where
      chunk = DVU.take 8 bytes
      outString = outF chunk
