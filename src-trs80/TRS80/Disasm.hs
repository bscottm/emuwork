{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
TRS-80 Model I disassembler.
-}

module TRS80.Disasm
  ( trs80disassemble
  ) where

import           Control.Arrow                    (first)
import           Control.Monad                    (unless)
import           Control.Monad.Trans.State.Strict (runState, state)

import           Data.Binary
import           Data.Bits
import qualified Data.ByteString.Lazy             as B
import           Data.Char
import           Data.Digest.Pure.MD5
import qualified Data.Foldable                    as Foldable
import qualified Data.HashMap.Strict              as H
import           Data.Maybe
import           Data.Sequence                    ((><), (|>))
import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import qualified Data.Vector                      as V
import           Data.Vector.Unboxed              (Vector, (!))
import qualified Data.Vector.Unboxed              as DVU

import           Lens.Micro.Platform              ((%~), (&), (.~), (^.))

import           System.IO

import           Machine
import qualified TRS80.Disasm.Guidance as G
import           TRS80.System
import           Z80

-- import           Debug.Trace

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassemble the TRS-80 ROM (with annotations, known symbols, ...)
trs80disassemble :: Z80system z80sys
                 -> (FilePath -> IO (Vector Word8))
                 -> FilePath
                 -> Int
                 -> G.Guidance
                 -> IO ()
trs80disassemble sys imgReader imgName msize guidance =
  do
    trs80 <- trs80System imgName imgReader msize sys
    let (img, _sys') = sysMReadN theOrigin (fromIntegral (theEndAddr - theOrigin) + 1) trs80
    unless (DVU.null img) $
      case G.getMatchingSection guidance (romMD5 img) of
        Just guidance' ->
          do
            let dstate = initialDisassembly trs80 guidance'
                (disSeq, finalDState) = iterateGuidance guidance' dstate
            banner img dstate
            mapM_ (z80AnalyticDisassemblyOutput stdout finalDState) disSeq
            mapM_ TIO.putStrLn (z80FormatSymbolTable finalDState)
        Nothing ->
          mapM_ (TIO.hPutStrLn stderr)
            [ T.append "Could not find guidance section for " (romMD5Hex img)
            , T.append "Origin = " (as0xHex theOrigin)
            , T.append "End addr = " (as0xHex theEndAddr)
            , T.append "img length = " $ (T.pack . show) (DVU.length img)
            ]
  where
    theOrigin                   = getGuidanceAddrDefault (G.origin guidance)
    theEndAddr                  = getGuidanceAddrDefault (G.endAddr guidance)
    -- Initial disassembly state
    initialDisassembly sys' guide' =
      mkDisassemblyState sys' theOrigin theEndAddr
        & disasmSymbolTable .~ G.invertKnownSymbols guide'
        & disasmPostProc    .~ trs80RomPostProcessor
    -- Self explanitory
    iterateGuidance guidance' {-dstate-} = runState (sequence [state (doDirective g) | g <- V.toList guidance'])
    -- Self explanitory
    banner img dstate'=
      z80AnalyticDisassemblyOutput stdout dstate' $
        Seq.empty |> mkLineComment commentBreak
                  |> mkLineComment (T.append "ROM MD5 checksum: " (romMD5Hex img))
                  |> mkLineComment ((identifyROM . romMD5) img)
                  |> mkLineComment commentBreak
                  |> mkLineComment T.empty
    commentBreak           = "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
    romMD5                 = encode . md5 . B.pack . DVU.toList
    romMD5Hex img          = B.foldr (T.append . asHex) T.empty (romMD5 img)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

getGuidanceAddr :: DisasmState cpuType insnType Z80addr wordType extPseudoType
                -> G.Z80guidanceAddr
                -> ProgramCounter Z80addr
getGuidanceAddr dstate addr =
  case addr of
    G.FromCurPC -> dstate ^. disasmCurAddr
    G.GuidanceAddr addr' -> PC addr'

getGuidanceAddrDefault :: G.Z80guidanceAddr
                       -> Z80addr
getGuidanceAddrDefault addr =
  case addr of
    G.FromCurPC -> error "getGuidanceAddrDefault: No default for FromCurPC."
    G.GuidanceAddr addr' -> addr'

getGuidanceRange :: DisasmState cpuType insnType Z80addr wordType extPseudoType
                 -> G.Z80guidanceAddrRange
                 -> (ProgramCounter Z80addr, ProgramCounter Z80addr)
getGuidanceRange dstate range =
  case range of
    G.AbsRange sAddr eAddr -> (getGuidanceAddr dstate sAddr, getGuidanceAddr dstate eAddr)
    G.RelRange sAddr disp  ->
      let sAddr' = getGuidanceAddr dstate sAddr
          eAddr' = sAddr' + (fromIntegral . G.unZ80guidanceDisp) disp
      in  (sAddr', eAddr')

getGuidanceRangeDisp :: DisasmState cpuType insnType Z80addr wordType extPseudoType
                     -> G.Z80guidanceAddrRange
                     -> (ProgramCounter Z80addr, Z80disp)
getGuidanceRangeDisp dstate range =
  case range of
    G.AbsRange sAddr eAddr ->
      let sAddr' = getGuidanceAddr dstate sAddr
          eAddr' = getGuidanceAddr dstate eAddr
      in  (sAddr', fromIntegral (eAddr' - sAddr'))
    G.RelRange sAddr disp  -> (getGuidanceAddr dstate sAddr, G.unZ80guidanceDisp disp)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Identify a ROM from its MD5 signature.
identifyROM :: B.ByteString
            -> T.Text
identifyROM md5sig = fromMaybe "Unknown ROM signature" (md5sig `H.lookup` romSigs)

romSigs :: H.HashMap B.ByteString T.Text
romSigs = H.fromList [ ( B.pack [ 0xca, 0x74, 0x82, 0x2e, 0xbc, 0x28, 0x03, 0xc6, 0x63, 0x5a, 0x55, 0x11
                                , 0x6e, 0xcd, 0x95, 0x39 ]
                       , "Model I v1.2-3a" )
                     -- This one is pretty elusive in the wild...
                     , ( B.pack [ 0x6f, 0x0a, 0xc8, 0x17, 0x9f, 0xa0, 0x1c, 0xc4, 0x47, 0x20, 0xda, 0x31
                                , 0x9c, 0xe1, 0x2a, 0x92 ]
                       , "Model I v1.3-1" )
                     , ( B.pack [ 0x6c, 0x73, 0x4a, 0x96, 0x36, 0x5b, 0xae, 0x81, 0xc7, 0x29, 0xc2, 0xe4
                                , 0xf0, 0xf7, 0x02, 0x0e ]
                       , "System-80 blue label" )
                     ]

-- | Interpret and execute a Z80 disassembly guidance directive
doDirective :: G.Directive
            -> Z80disassembly
            -> (Seq.Seq Z80DisasmElt, Z80disassembly)

doDirective (G.SymEquate label addr) dstate =
  let addr' = unPC $ getGuidanceAddr dstate addr
  in  (Seq.singleton $ mkEquate label addr', dstate & disasmSymbolTable %~ H.insert addr' label)

doDirective (G.Comment comment) dstate =
  (Seq.singleton $ mkLineComment comment, dstate)

doDirective (G.DoDisasm addrRange) dstate =
  let (sAddr, eAddr) = getGuidanceRange dstate addrRange
  in  disassembler (dstate & disasmCurAddr .~ sAddr & disasmFinishAddr .~ eAddr)

doDirective (G.GrabBytes addrRange) dstate =
  let (sAddr, nBytes) = getGuidanceRangeDisp dstate addrRange
  in  first Seq.singleton $ z80disbytes nBytes (dstate & disasmCurAddr .~ sAddr)

doDirective (G.GrabAsciiZ sAddr) dstate =
  z80disasciiz (dstate & disasmCurAddr .~ getGuidanceAddr dstate sAddr)

doDirective (G.GrabAscii addrRange) dstate =
  let (sAddr, nBytes) = getGuidanceRangeDisp dstate addrRange
  in  first Seq.singleton $ z80disascii nBytes (dstate & disasmCurAddr .~ sAddr)

doDirective (G.HighBitTable addrRange) dstate =
  let (sAddr, nBytes) = getGuidanceRangeDisp dstate addrRange
  in  highbitCharTable nBytes (dstate & disasmCurAddr .~ sAddr)

doDirective (G.JumpTable addrRange) dstate =
  let (sAddr, nBytes) = getGuidanceRangeDisp dstate addrRange
  in  jumpTable nBytes (dstate & disasmCurAddr .~ sAddr)

doDirective (G.MD5Sum _) dstate =
  (Seq.empty, dstate)

doDirective (G.KnownSymbols _) dstate =
  (Seq.empty, dstate)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | The TRS-80's BASIC has a table of keywords, where the first letter of the keyword has the
-- high bit on. Scan through and generate all of the pseudo-operations for this table, within
-- the specified memory range.
highbitCharTable :: Z80disp
                 -- ^ Number of bytes to extract
                 -> Z80disassembly
                 -- ^ Current disassembly state
                 -> (Seq.Seq Z80DisasmElt, Z80disassembly)
                 -- ^ Resulting diassembly state
highbitCharTable nBytes z80dstate =
  let sAddr               = fromIntegral (z80dstate ^. disasmCurAddr)
      -- Fetch the block from memory as a 'Vector'
      (memBlock, z80dstate') = disasmMReadN (fromIntegral nBytes) z80dstate
      -- Look for the high bit characters within the address range, then convert back to addresses
      byteidxs            = DVU.findIndices (>= 0x80) memBlock
      -- Set up a secondary index vector to make a working zipper
      byteidx2            = DVU.drop 1 byteidxs `DVU.snoc` fromIntegral nBytes
      -- Grab an individual string from a memory range
      -- grabString :: Int -> Int -> Seq Z80DisasmElt
      grabString memidx memidx' =
        let theChar = memBlock ! memidx
            theChar' = fromIntegral theChar :: Int
            firstByte = T.concat [ "'"
                                 , T.singleton (chr (theChar' .&. 0x7f))
                                 , "' .|. 80H"
                                 ]
            firstBytePseudo = ExtPseudo (ByteExpression (fromIntegral (sAddr + memidx)) firstByte theChar)
            theString = mkAscii (fromIntegral $ sAddr + memidx + 1)
                                (DVU.slice (memidx + 1) (memidx' - memidx - 1) memBlock)
        in  if (memidx + 1) /= memidx' then
              Seq.singleton firstBytePseudo |> theString
            else
              Seq.singleton firstBytePseudo
      -- Zip the two index vectors to a sequence
      disasmElts   = Foldable.foldr (><) Seq.empty (zipWith grabString (DVU.toList byteidxs) (DVU.toList byteidx2))
  in  ( disasmElts
      , z80dstate'
      )

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

jumpTable :: Z80disp
          -- ^ Number of bytes to extract
          -> Z80disassembly
          -- ^ Current disassembly state
          -> (Seq.Seq Z80DisasmElt, Z80disassembly)
          -- ^ Resulting diassembly state
jumpTable nBytes dstate = first Seq.fromList $ runState (sequence [state genAddr | _elt <- [0..(nBytes - 2) `div` 2]]) dstate
  where
    genAddr dstate'        = first (genPseudo dstate') $ disasmMReadN 2 dstate'
    genPseudo dstate' addr = mkAddr (unPC (dstate' ^. disasmCurAddr)) ((AbsAddr . flipWords) addr) addr
    flipWords addr         = shiftL (fromIntegral (addr DVU.! 1)) 8 .|. fromIntegral (addr DVU.! 0)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Post-processing for RST 8 "macros" in the TRS-80 ROM. RST 08 is always followed
-- by a character; (HL) is compared to this following character and flags set.
trs80RomPostProcessor :: DisElementPostProc Z80state Z80instruction Z80addr Z80word Z80PseudoOps
trs80RomPostProcessor rst08@(DisasmInsn _ _ (RST 8) _) dstate =
  let curPC = unPC (dstate ^. disasmCurAddr)
      (byte, dstate') = disasmMRead dstate
      -- Ensure that the next byte is printable ASCII, otherwise disassemble as a byte.
      pseudo = if byte >= 0x20 && byte <= 0x7f then mkAscii else mkByteRange
  in  (Seq.singleton rst08 |> pseudo curPC (DVU.singleton byte), dstate')
-- Otherwise, just append the instruction onto the disassembly sequence.
trs80RomPostProcessor elt dstate = z80DefaultPostProcessor elt dstate

{-
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
-}
