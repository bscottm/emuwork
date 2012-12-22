-- | The venerable TRS-80 Level II ROM disassembly module.
--
-- This module is merely a driver to disassemble the TRS-80 Level II ROM, complete with annotations
module Main where

import System.IO (stdout, stderr, hPutStrLn)
import System.Environment
import System.Console.GetOpt
import System.Exit
import Control.Lens
import Control.Monad
import Data.Digest.Pure.MD5
import Data.Binary

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BCL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Foldable as Foldable
import Data.Bits
import Data.Char

-- import Debug.Trace

import Reader
import Machine
import Z80

import Guidance
import KnownSymbols

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassemble the TRS-80 ROM (with annotations, known symbols, ...)
trs80Rom :: (FilePath -> IO (Vector Word8))
         -> String 
         -> IO ()
trs80Rom imgReader imgName =
  imgReader imgName
  >>= (\img -> let dis = collectRom (romMemory img) (initialDisassembly img) actions
               in  if (not . DVU.null) img then
                     do
                       checkAddrContinuity dis
                       >> z80AnalyticDisassemblyOutput stdout dis
                   else
                     return ()
      )
  where
    -- Initial disassembly state: known symbols and disassembly address range predicate
    initialDisassembly img = disasmSeq %~ (\s -> s |> (LineComment commentBreak)
                                                   |> (LineComment (T.append "ROM MD5 checksum: " (romMD5Hex img)))
                                                   |> (LineComment ((identifyROM . romMD5) img))
                                                   |> (LineComment commentBreak)
                                                   |> (LineComment T.empty)) $
                               symbolTab .~ knownSymbols $
                               addrInDisasmRange .~ trs80RomRange $
                               mkInitialDisassembly
    commentBreak           = "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
    romMD5 img             = (encode . md5 . BCL.pack . DVU.toList) img
    romMD5Hex img          = BCL.foldl (\a x -> T.append a (asHex x)) T.empty (romMD5 img)
    lastAddr               = 0x2fff
    -- ROM range for 'addrInDisasmRange' predicate
    trs80RomRange addr     = addr >= 0x0000 && addr <= lastAddr

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Identify a ROM from its MD5 signature.
identifyROM :: BCL.ByteString
            -> T.Text
identifyROM md5sig = case Map.lookup md5sig romSigs of
                       Nothing      -> "Unknown ROM signature"
                       Just romName -> romName

romSigs :: Map.Map BCL.ByteString T.Text
romSigs = Map.fromList [ ( BCL.pack [ 0xca, 0x74, 0x82, 0x2e, 0xbc, 0x28, 0x03, 0xc6, 0x63, 0x5a, 0x55, 0x11
                                    , 0x6e, 0xcd, 0x95, 0x39 ]
                         , "Model I v1.2-3a" )
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
collectRom :: Z80memory (Vector Z80word)                -- ^ The system's memory
           -> Z80disassembly                            -- ^ Initial disassembler state. This is pre-populated with known
                                                        -- symbols in the symbol table.
           -> [Guidance]                                -- ^ Disassembler guidance
           -> Z80disassembly                            -- ^ Resulting disassembler state, symbol table and disassembly
                                                        -- sequence.
collectRom img = Foldable.foldl' (doAction img)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

doAction :: Z80memory (Vector Z80word)
         -> Z80disassembly
         -> Guidance
         -> Z80disassembly

doAction mem dstate guide
  {-  | trace ("disasm: guide = " ++ (show guide)) False = undefined -}
  | (SetOrigin origin)         <- guide = disasmSeq %~ (|> (DisOrigin origin)) $ dstate
  | (SymEquate label addr)     <- guide = 
    (symbolTab %~ (Map.insert addr label)) . (disasmSeq %~ (|> (Equate label addr))) $ dstate
  | (Comment comment)          <- guide = disasmSeq %~ (|> (LineComment comment)) $ dstate
  | (DoDisasm sAddr nBytes)    <- guide = disassemble dstate mem (PC $ sAddr) (PC $ sAddr + fromIntegral nBytes)
                                                      trs80RomPostProcessor
  | (GrabBytes sAddr nBytes)   <- guide = z80disbytes dstate mem (PC $ sAddr) nBytes
  | (GrabAsciiZ sAddr)         <- guide = z80disasciiz dstate mem (PC $ sAddr)
  | (GrabAscii sAddr nBytes)   <- guide = z80disascii dstate mem (PC $ sAddr) nBytes
  | (HighBitTable addr nBytes) <- guide = highbitCharTable mem addr nBytes dstate
  | (JumpTable addr nBytes)    <- guide = jumpTable mem addr nBytes dstate
  | otherwise                           = dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | The TRS-80's BASIC has a table of keywords, where the first letter of the keyword has the
-- high bit on. Scan through and generate all of the pseudo-operations for this table, within
-- the specified memory range.
highbitCharTable :: Z80memory (Vector Z80word)  -- ^ Vector of bytes from which to extract some data
                 -> Z80addr                     -- ^ Start address, relative to the origin, to start extracting bytes
                 -> Z80disp                     -- ^ Number of bytes to extract
                 -> Z80disassembly              -- ^ Current disassembly state
                 -> Z80disassembly              -- ^ Resulting diassembly state
highbitCharTable mem sAddr nBytes z80dstate =
  let sAddr'   = fromIntegral sAddr
      nBytes'  = fromIntegral nBytes
      -- Fetch the block from memory as a 'Vector'
      memBlock = (mem ^. mfetchN) sAddr nBytes'
      -- Look for the high bit characters within the address range, then convert back to addresses
      byteidxs = DVU.findIndices (\x -> x >= 0x80) memBlock
      -- Set up a secondary index vector to make a working zipper
      byteidx2 = (DVU.drop 1 byteidxs) `DVU.snoc` nBytes'
      -- Grab an individual string from a memory range
      grabString :: Int -> Int -> Seq Z80DisasmElt
      grabString memidx memidx' =
        let theChar = memBlock ! memidx
            theChar' = fromIntegral theChar :: Int
            firstByte = T.concat [ "'"
                                 , T.singleton (chr (theChar' .&. 0x7f))
                                 , "' .|. 80H"
                                 ]
            firstBytePseudo = ExtPseudo (ByteExpression (fromIntegral (sAddr' + memidx)) firstByte theChar)
            theString = Ascii (fromIntegral $ sAddr' + memidx + 1)
                              (DVU.slice (memidx + 1) (memidx' - memidx - 1) memBlock)
        in  if (memidx + 1) /= memidx' then
              Seq.singleton firstBytePseudo |> theString
            else
              Seq.singleton firstBytePseudo
      -- Zip the two index vectors to a sequence
      disasmElts   = Foldable.foldl' (><) Seq.empty (zipWith grabString (DVU.toList byteidxs) (DVU.toList byteidx2))
  in  disasmSeq %~ (>< disasmElts) $ z80dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

jumpTable :: Z80memory (Vector Z80word) -- ^ Vector of bytes from which to extract some data
          -> Z80addr                    -- ^ Start address, relative to the origin, to start extracting bytes
          -> Z80disp                    -- ^ Number of bytes to extract
          -> Z80disassembly             -- ^ Current disassembly state
          -> Z80disassembly             -- ^ Resulting diassembly state
jumpTable mem sAddr nBytes dstate =
  let endAddr = sAddr + (fromIntegral nBytes)
      generateAddr addr z80dstate
        | addr < endAddr - 2  = let (newAddr, operand) = z80DisGetAddr mem (PC addr)
                                in  generateAddr (getPCvalue newAddr) (operAddrPseudo operand)
        | addr == endAddr - 2 = operAddrPseudo $ _2 ^$ (z80DisGetAddr mem (PC addr))
        | otherwise           = z80disbytes z80dstate mem (PC addr) (fromIntegral $ endAddr - addr)
        where
          operAddrPseudo theAddr = disasmSeq %~ (|> (operAddr theAddr)) $ z80dstate
          operAddr theAddr = 
            let symTab = view symbolTab z80dstate
                oper   = if Map.member theAddr symTab then
                           SymAddr $ symTab Map.! theAddr
                         else
                           AbsAddr theAddr
                bytes  = (mem ^. mfetchN) addr 2
            in  Addr addr oper bytes
  in  generateAddr sAddr dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Post-processing for RST 8 "macros" in the TRS-80 ROM. RST 08 is always followed
-- by a character; (HL) is compared to this following character and flags set.
trs80RomPostProcessor :: Z80DisasmElt
                      -> Z80memory (Vector Z80word)
                      -> Z80PC
                      -> Z80disassembly
                      -> (Z80PC, Z80disassembly)
trs80RomPostProcessor ins@(DisasmInsn _ _ (RST 8) _) mem pc dstate =
  let sAddr  = getPCvalue pc
      byte   = (mem ^. mfetch) sAddr
      -- Ensure that the next byte is printable ASCII, otherwise disassemble as a byte.
      pseudo = if byte >= 0x20 && byte <= 0x7f then
                 Ascii
               else
                 ByteRange
  in  (pcInc pc, (disasmSeq %~ (\s -> s |> ins |> (pseudo sAddr (DVU.singleton byte))) $ dstate))
-- Otherwise, just append the instruction onto the disassembly sequence.
trs80RomPostProcessor elt mem pc dstate = z80DefaultPostProcessor elt mem pc dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Primitive memory system for the raw image ROMs.
romMemory :: Vector Z80word
          -> Z80memory (Vector Z80word)
romMemory imgdata =
  let rom = MemorySystem
            { _memInternals = imgdata
            , _mfetch       = (\addr -> imgdata ! (fromIntegral addr))
            , _mfetchN      = (\addr nBytes -> DVU.slice (fromIntegral addr) nBytes imgdata)
            , _maxmem       = fromIntegral . DVU.length $ imgdata
            }
  in  rom

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Check the disassembled sequence for address continuity to ensure that the addresses
-- increase monotonically, without gaps.
checkAddrContinuity :: Z80disassembly
                    -> IO ()
checkAddrContinuity dis =
  let insOnly   = dis ^. disasmSeq ^& (Seq.filter isZ80AddrIns)
      insAddrs  = fmap z80InsAddr insOnly
      nextAddrs = Seq.zipWith (+) (fmap (fromIntegral . z80InsLength) insOnly) insAddrs
      nextAddrs'  = (Seq.drop 1 insAddrs) |> (fromIntegral $ Seq.index nextAddrs (Seq.length nextAddrs - 1))

      checkSeq   = Seq.zipWith (\a1 a2 -> a1 == a2) nextAddrs' nextAddrs

      formatDiscontinuity (expected, got) = 
        TIO.hPutStrLn stderr $ T.concat [ "  expected "
                                        , as0xHex expected
                                        , ", got "
                                        , as0xHex got
                                        ]
  in  if Foldable.and checkSeq then
        return ()
      else
        hPutStrLn stderr "Discontinuities = "
        >> (Foldable.traverse_ formatDiscontinuity $ Seq.filter (\(a, b) -> a /= b) $ (Seq.zip nextAddrs' nextAddrs))

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data UtilCommand =
  UtilCommand
  { imageReader :: FilePath -> IO (Vector Word8)
  }

mkUtilCommand :: UtilCommand
mkUtilCommand = UtilCommand
                { imageReader = readRawWord8Vector
                }

utilOptions :: [OptDescr (UtilCommand -> IO UtilCommand)]
utilOptions =
 [ Option [] ["format"] (ReqArg setImageReader "<Rom image format>") "ROM image reader ('ihex', 'intelhex' or 'raw')"
 ]
 where
  setImageReader fmt flags =
    case fmt of
      "ihex"     -> return $ flags { imageReader = readIntelHexVector }
      "intelhex" -> return $ flags { imageReader = readIntelHexVector }
      "raw"      -> return $ flags { imageReader = readRawWord8Vector }
      unknown    -> hPutStrLn stderr ("Unknown reader format: '" ++ (show unknown) ++ "'")
                    >> hPutStrLn stderr "Valid formats are 'ihex', 'intelhex' or 'raw'"
                    >> showUsage
                    >> return flags

showUsage :: IO ()
showUsage = do
  prog <- getProgName
  hPutStrLn stderr ("Usage: " ++ prog ++ " <--format=(ihex|intelhex|raw)> image")
  exitFailure

main :: IO ()
main =
    getArgs
    >>= return . getOpt RequireOrder utilOptions
    >>= (\(optsActions, rest, errs) ->
          (unless (null errs) $ do
             mapM_ (hPutStrLn stderr) errs
             >> showUsage
             >> exitFailure)
          >> Foldable.foldl' (>>=) (return mkUtilCommand) optsActions
          >>= (\options ->
                if length rest == 1 then
                  trs80Rom (imageReader options) (head rest)
                else
                  showUsage))
