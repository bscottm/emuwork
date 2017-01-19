{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
TRS-80 Model I disassembler.
-}

module TRS80.Disasm
  ( disasmCmd
  , disasmUsage
  ) where

import           Control.Arrow (second)
import           Control.Lens (over, (^.), (%~), (.~), (&))
import           Control.Monad (unless, mapM_)
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString.Lazy as BCL
import           Data.Char
import           Data.Digest.Pure.MD5
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Data.Sequence ((|>), (><))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Yaml as Y
-- import           Debug.Trace
import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           Machine
import           TRS80.CommonOptions
import qualified TRS80.Disasm.Guidance as G
import           TRS80.System
import           Z80

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassembler command main logic.
disasmCmd :: Z80system z80sys
          -> [String]
          -> IO ()
disasmCmd sys opts =
  do
    options <- getCommonOptions opts
    case options of
      (CommonOptions imgRdr image msize, _rest, unOpts) ->
        do
          disopts <- getDisasmOptions unOpts
          case disopts of
            (DisasmOptions gFile, []) ->
              do
                gresult <- Y.decodeFileEither gFile :: IO (Either Y.ParseException G.Guidance)
                case gresult of
                  Right guidance -> trs80disassemble sys imgRdr image msize guidance
                  Left  err      -> mapM_ (hPutStrLn stderr)
                                      [ gFile ++ ":"
                                      , Y.prettyPrintParseException err
                                      ]

            (_, _) -> hPutStrLn stderr "Invalid disassembler options. Exiting."
                      >> showUsage
      (InvalidOptions, _, _) ->
          disasmUsage
          >> exitFailure

      (_, rest, unOpts) ->
        hPutStrLn stderr "Unrecognized options:"
        >> mapM_ (\s -> hPutStrLn stderr ("  " ++ s)) unOpts
        >> hPutStrLn stderr ("Extra arguments: " ++ show rest)
        >> showUsage
  where
    showUsage = commonOptionUsage
                >> disasmUsage
                >> exitFailure

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data DisasmOptions where
  -- | Disassembler-specific options
  DisasmOptions ::
    { guidanceFile :: FilePath
    } -> DisasmOptions
  InvalidDisasm :: DisasmOptions

getDisasmOptions :: [String]
                 -> IO (DisasmOptions, [String])
getDisasmOptions opts =
  case getOpt Permute disasmOptions opts of
    (optsActions, rest, [])   -> return (Foldable.foldl (flip id) mkDisasmOptions optsActions, rest)
    (_,           _,    errs) -> mapM_ (hPutStrLn stderr) errs
                                 >> return (InvalidDisasm, [])

mkDisasmOptions :: DisasmOptions
mkDisasmOptions = DisasmOptions { guidanceFile = "" }

disasmOptions :: [OptDescr (DisasmOptions -> DisasmOptions)]
disasmOptions =
  [ Option [] ["guidance"] (ReqArg (\arg flags -> flags { guidanceFile = arg }) "FILE")
                           "Disassembler guidance file"
  ]

disasmUsage :: IO ()
disasmUsage = hPutStrLn stderr (usageInfo "Disassembler options" disasmOptions)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassemble the TRS-80 ROM (with annotations, known symbols, ...)
trs80disassemble :: Z80system z80sys
                 -> (FilePath -> IO (Vector Word8))
                 -> FilePath
                 -> Int
                 -> G.Guidance
                 -> IO ()
trs80disassemble sys imgReader imgName msize guidance =
  trs80System imgName imgReader (fromIntegral msize) sys
  >>= (\trs80 -> let (img, _msys') = mReadN theOrigin (fromIntegral (theEndAddr - theOrigin) + 1) (trs80 ^. memory)
                 in  case G.getMatchingSection guidance (romMD5 img) of
                       Just dirs ->
                         let (dis, _msys) = collectRom trs80 (initialDisassembly img dirs) dirs
                         in  unless (DVU.null img) $
                               checkAddrContinuity dis
                                >> z80AnalyticDisassemblyOutput stdout dis
                       Nothing ->
                        mapM_ (hPutStrLn stderr)
                          [ "Could not find guidance section for " ++ T.unpack (romMD5Hex img)
                          , "Origin = " ++ as0xHexS theOrigin
                          , "End addr = " ++ as0xHexS theEndAddr
                          , "img length = " ++ show (DVU.length img)
                          ]
      )
  where
    theOrigin              = G.origin guidance
    theEndAddr             = G.endAddr guidance
    -- Initial disassembly state: known symbols and disassembly address range predicate
    initialDisassembly img dirs =
      disasmSeq %~ (\s -> s |> mkLineComment commentBreak
                            |> mkLineComment (T.append "ROM MD5 checksum: " (romMD5Hex img))
                            |> mkLineComment ((identifyROM . romMD5) img)
                            |> mkLineComment commentBreak
                            |> mkLineComment T.empty) $
                   symbolTab .~ fromMaybe H.empty (G.getKnownSymbols dirs) $
                   addrInDisasmRange .~ (\addr -> addr >= theOrigin && addr <= theEndAddr) $
                   mkInitialDisassembly
    commentBreak           = "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
    romMD5                 = encode . md5 . BCL.pack . DVU.toList
    romMD5Hex img          = BCL.foldl (\a x -> T.append a (asHex x)) T.empty (romMD5 img)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Identify a ROM from its MD5 signature.
identifyROM :: BCL.ByteString
            -> T.Text
identifyROM md5sig = fromMaybe "Unknown ROM signature" (md5sig `H.lookup` romSigs)

romSigs :: H.HashMap BCL.ByteString T.Text
romSigs = H.fromList [ ( BCL.pack [ 0xca, 0x74, 0x82, 0x2e, 0xbc, 0x28, 0x03, 0xc6, 0x63, 0x5a, 0x55, 0x11
                                  , 0x6e, 0xcd, 0x95, 0x39 ]
                       , "Model I v1.2-3a" )
                     -- This one is pretty elusive in the wild...
                     , ( BCL.pack [ 0x6f, 0x0a, 0xc8, 0x17, 0x9f, 0xa0, 0x1c, 0xc4, 0x47, 0x20, 0xda, 0x31
                                 , 0x9c, 0xe1, 0x2a, 0x92 ]
                       , "Model I v1.3-1" )
                     , ( BCL.pack [ 0x6c, 0x73, 0x4a, 0x96, 0x36, 0x5b, 0xae, 0x81, 0xc7, 0x29, 0xc2, 0xe4
                                  , 0xf0, 0xf7, 0x02, 0x0e ]
                       , "System-80 blue label" )
                     ]

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Collect the TRS-80 ROM (oh, this could sooo be generalized!) by left folding the disassembler guidance
-- and collecing the disassembler state, symbol table and disassembled instruction sequence.
collectRom :: Z80system TRS80ModelISystem
              -- ^ The TRS-80 system
           -> Z80disassembly
              -- ^ Initial disassembler state. This is pre-populated with known
              -- symbols in the symbol table.
           -> V.Vector G.Directive
              -- ^ Disassembler guidance
           -> (Z80disassembly, Z80system TRS80ModelISystem)
              -- ^ Resulting disassembler state, symbol table and disassembly
              -- sequence.
collectRom sys dstate = Foldable.foldl doAction (dstate, sys)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

doAction :: (Z80disassembly, Z80system z80sys)
         -> G.Directive
         -> (Z80disassembly, Z80system z80sys)

doAction (dstate, sys) guide
  --  | trace ("disasm: guide = " ++ (show guide)) False = undefined
  | (G.SymEquate label addr)     <- guide
  = ((symbolTab %~ H.insert addr label) . (disasmSeq %~ (|> mkEquate label addr)) $ dstate, sys)
  | (G.Comment comment)          <- guide
  = (disasmSeq %~ (|> mkLineComment comment) $ dstate, sys)
  | (G.DoDisasm sAddr nBytes)    <- guide
  = second (updMem sys) (disassemble dstate sys (PC sAddr) (PC sAddr + fromIntegral nBytes) trs80RomPostProcessor)
  | (G.GrabBytes sAddr nBytes)   <- guide
  = second (updMem sys) (z80disbytes dstate mem (PC sAddr) nBytes)
  | (G.GrabAsciiZ sAddr)         <- guide
  = second (updMem sys) (z80disasciiz dstate mem (PC sAddr))
  | (G.GrabAscii sAddr nBytes)   <- guide
  = second (updMem sys) (z80disascii dstate mem (PC sAddr) nBytes)
  | (G.HighBitTable addr nBytes) <- guide
  = second (updMem sys) (highbitCharTable mem addr nBytes dstate)
  | (G.JumpTable addr nBytes)    <- guide
  = second (updMem sys) (jumpTable mem addr nBytes dstate)
  | otherwise
  = (dstate, sys)
  where
    updMem sys' mem' = sys' & memory .~ mem'
    mem = sys ^. memory

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | The TRS-80's BASIC has a table of keywords, where the first letter of the keyword has the
-- high bit on. Scan through and generate all of the pseudo-operations for this table, within
-- the specified memory range.
highbitCharTable :: Z80memory
                 -- ^ Vector of bytes from which to extract some data
                 -> Z80addr
                 -- ^ Start address, relative to the origin, to start extracting bytes
                 -> Z80disp
                 -- ^ Number of bytes to extract
                 -> Z80disassembly
                 -- ^ Current disassembly state
                 -> (Z80disassembly, Z80memory)
                 -- ^ Resulting diassembly state
highbitCharTable memSys sAddr nBytes z80dstate =
  let sAddr'              = fromIntegral sAddr
      nBytes'             = fromIntegral nBytes
      -- Fetch the block from memory as a 'Vector'
      (memBlock, memSys') = mReadN sAddr nBytes' memSys
      -- Look for the high bit characters within the address range, then convert back to addresses
      byteidxs            = DVU.findIndices (>= 0x80) memBlock
      -- Set up a secondary index vector to make a working zipper
      byteidx2            = DVU.drop 1 byteidxs `DVU.snoc` nBytes'
      -- Grab an individual string from a memory range
      -- grabString :: Int -> Int -> Seq Z80DisasmElt
      grabString memidx memidx' =
        let theChar = memBlock ! memidx
            theChar' = fromIntegral theChar :: Int
            firstByte = T.concat [ "'"
                                 , T.singleton (chr (theChar' .&. 0x7f))
                                 , "' .|. 80H"
                                 ]
            firstBytePseudo = ExtPseudo (ByteExpression (fromIntegral (sAddr' + memidx)) firstByte theChar)
            theString = mkAscii (fromIntegral $ sAddr' + memidx + 1)
                                (DVU.slice (memidx + 1) (memidx' - memidx - 1) memBlock)
        in  if (memidx + 1) /= memidx' then
              Seq.singleton firstBytePseudo |> theString
            else
              Seq.singleton firstBytePseudo
      -- Zip the two index vectors to a sequence
      disasmElts   = Foldable.foldl (><) Seq.empty (zipWith grabString (DVU.toList byteidxs) (DVU.toList byteidx2))
  in  (disasmSeq %~ (>< disasmElts) $ z80dstate, memSys')

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

jumpTable :: Z80memory
          -- ^ Vector of bytes from which to extract some data
          -> Z80addr
          -- ^ Start address, relative to the origin, to start extracting bytes
          -> Z80disp
          -- ^ Number of bytes to extract
          -> Z80disassembly
          -- ^ Current disassembly state
          -> (Z80disassembly, Z80memory)
          -- ^ Resulting diassembly state
jumpTable mem sAddr nBytes dstate =
  let ea = sAddr + fromIntegral nBytes
      generateAddr mem' addr z80dstate
        | addr < ea - 2  = let (DecodedAddr newAddr operand, mem'')  = z80getAddr mem' (PC addr)
                               (dstate'', mem''') = operAddrPseudo operand mem''
                           in  generateAddr mem''' (unPC newAddr) dstate''
        | addr == ea - 2 = let (DecodedAddr _newAddr operand, mem'') = z80getAddr mem' (PC addr)
                           in  operAddrPseudo operand mem''
        | otherwise      = z80disbytes z80dstate mem' (PC addr) (fromIntegral (ea - addr))
        where
          operAddrPseudo theAddr msys = let (addrPseudo, msys') = operAddr theAddr msys
                                        in  (over disasmSeq (|> addrPseudo) z80dstate, msys')
          operAddr       theAddr msys = let (addrVec, msys') = mReadN addr 2 msys
                                        in  (mkAddr addr (AbsAddr theAddr) addrVec, msys')
  in  generateAddr mem sAddr dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Post-processing for RST 8 "macros" in the TRS-80 ROM. RST 08 is always followed
-- by a character; (HL) is compared to this following character and flags set.
trs80RomPostProcessor :: Z80DisasmElt
                      -> Z80memory
                      -> Z80PC
                      -> Z80disassembly
                      -> (Z80PC, Z80disassembly, Z80memory)
trs80RomPostProcessor ins@(DisasmInsn _ _ (RST 8) _) memSys pc dstate =
  let (byte, memSys') = mRead (unPC pc) memSys
      -- Ensure that the next byte is printable ASCII, otherwise disassemble as a byte.
      pseudo = if byte >= 0x20 && byte <= 0x7f then
                 mkAscii
               else
                 mkByteRange
  in  (pc + 1, disasmSeq %~ (\s -> s |> ins |> pseudo (unPC pc) (DVU.singleton byte)) $ dstate, memSys')
-- Otherwise, just append the instruction onto the disassembly sequence.
trs80RomPostProcessor elt mem pc dstate = z80DefaultPostProcessor elt mem pc dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Check the disassembled sequence for address continuity to ensure that the addresses
-- increase monotonically, without gaps.
checkAddrContinuity :: Z80disassembly
                    -> IO ()
checkAddrContinuity dis =
  let insOnly   = dis ^. disasmSeq & Seq.filter isZ80AddrIns
      insAddrs  = fmap z80InsAddr insOnly
      nextAddrs = Seq.zipWith (+) (fmap (fromIntegral . z80InsLength) insOnly) insAddrs
      nextAddrs'  = Seq.drop 1 insAddrs |> fromIntegral (Seq.index nextAddrs (Seq.length nextAddrs - 1))

      checkSeq   = Seq.zipWith (==) nextAddrs' nextAddrs

      formatDiscontinuity (expected, got) =
        TIO.hPutStrLn stderr $ T.concat [ "  expected "
                                        , as0xHex expected
                                        , ", got "
                                        , as0xHex got
                                        ]
  in  unless (Foldable.and checkSeq) $
        hPutStrLn stderr "Discontinuities = "
        >> Foldable.traverse_ formatDiscontinuity (Seq.filter (uncurry (/=)) (Seq.zip nextAddrs' nextAddrs))
