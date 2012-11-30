{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | The venerable TRS-80 Level II ROM disassembly module.
--
-- This module is merely a driver to disassemble the TRS-80 Level II ROM, complete with annotations
module Main where

import System.IO (stdout, stderr, hPutStrLn)
import System.Environment
import qualified Data.List as DL
import Control.Lens
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Sequence ((|>))
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as DVU
import Data.Bits
import Data.Char

-- import Language.Haskell.Pretty

-- import Reader.RawFormat
-- import Machine.DisassemblerTypes

import Reader
import Machine
import Z80

import Guidance
import KnownSymbols

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- Template Haskell hair for lenses
makeLenses ''Z80DisasmState

-- | Disassemble the TRS-80 ROM (with annotations, known symbols, ...)
trs80Rom :: String 
         -> IO ()
trs80Rom imgName = readRawWord8Vector imgName
                   >>= (\ img -> outputDisassembly stdout $ collectRom img initialDisassembly actions)
                   -- >> putStrLn (prettyPrint actions)
  where
    -- Initial disassembly state: known symbols and disassembly address range predicate
    initialDisassembly = Z80Disassembly $ set symbolTab knownSymbols $ set addrInDisasmRange trs80RomRange mkInitialDisassembly
    lastAddr = 0x2fff
    -- ROM range for 'addrInDisasmRange' predicate
    trs80RomRange addr = addr >= romOrigin && addr <= lastAddr

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

collectRom :: Z80memory
           -> Disassembly Z80DisasmState
           -> [Guidance]
           -> Disassembly Z80DisasmState
collectRom img dstate theActions = DL.foldl' (doAction img) dstate theActions

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

doAction :: Z80memory
         -> Disassembly Z80DisasmState
         -> Guidance
         -> Disassembly Z80DisasmState

doAction _img (Z80Disassembly dstate) (SetOrigin origin)               =
  Z80Disassembly $ disasmSeq %~ (|> DisasmPseudo (DisOrigin origin)) $ dstate

doAction _img (Z80Disassembly dstate) (AddAddrEquate label addr)       =
  Z80Disassembly $ (symbolTab %~ (Map.insert addr label)) .  (disasmSeq %~ (|> DisasmPseudo (AddrEquate label addr))) $ dstate

doAction _img (Z80Disassembly dstate) (AddLineComment comment)         =
  Z80Disassembly $ disasmSeq %~ (|> DisasmPseudo (LineComment comment)) $ dstate

doAction img dstate (DoDisasm origin sAddr nBytes)                     = disassemble img origin sAddr nBytes dstate
doAction img dstate (GrabBytes origin sAddr nBytes)                    = z80disbytes img origin sAddr nBytes dstate
doAction img dstate (GrabAsciiZ origin sAddr)                          = z80disasciiz img origin sAddr dstate
doAction img dstate (GrabAscii origin sAddr nBytes)                    = z80disascii img origin sAddr nBytes dstate
doAction img dstate (HighBitTable origin addr nBytes)                  = highbitCharTable img origin addr nBytes dstate
doAction img dstate (JumpTable origin addr nBytes)                     = jumpTable img origin addr nBytes dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | The TRS-80's BASIC has a table of keywords, where the first letter of the keyword has the
-- high bit on. Scan through and generate all of the pseudo-operations for this table, within
-- the specified memory range.
highbitCharTable :: Z80memory                   -- ^ Vector of bytes from which to extract some data
                 -> Z80addr                     -- ^ Output's origin address
                 -> Z80addr                     -- ^ Start address, relative to the origin, to start extracting bytes
                 -> Z80disp                     -- ^ Number of bytes to extract
                 -> Disassembly Z80DisasmState  -- ^ Current disassembly state
                 -> Disassembly Z80DisasmState  -- ^ Resulting diassembly state
highbitCharTable mem origin sAddr nBytes dstate =
  let sAddr'   = fromIntegral sAddr
      nBytes'  = fromIntegral nBytes
      -- Look for the high bit characters within the address range, then convert back to addresses
      byteidxs = DVU.map (\x -> x + sAddr') $ DVU.findIndices (\x -> x >= 0x80) $ DVU.slice sAddr' nBytes' mem
      -- Iterate through byteidxs' indices, grabbing the strings
      grabStrings idx theDState
        | idx == (DVU.length byteidxs) - 1 =
          -- last string to generate
          grabString (byteidxs ! idx) (sAddr' + nBytes' + 1) theDState
        | otherwise =
          let memidx  = byteidxs ! idx
              memidx' = byteidxs ! (idx + 1)
          in  grabStrings (idx + 1) $ grabString memidx memidx' theDState
      -- Grab an individual string from a memory range
      grabString memidx memidx' (Z80Disassembly z80dstate) =
        let theChar = mem ! memidx
            theChar' = fromIntegral theChar :: Int
            firstByte = BS.concat [ "'"
                                  , BS.singleton (chr (theChar' .&. 0x7f))
                                  , "' .|. 80H"
                                  ]
            firstBytePseudo = DisasmPseudo $ ByteExpression (origin + (fromIntegral memidx)) firstByte theChar
            theString = DisasmPseudo $ Ascii (origin + (fromIntegral memidx) + 1)
                                             (DVU.slice (memidx + 1) (memidx' - memidx - 1) mem)
        in  Z80Disassembly $ if (memidx + 1) /= memidx' then
                               disasmSeq %~ (|> theString) $ disasmSeq %~ (|> firstBytePseudo) $ z80dstate
                             else
                               disasmSeq %~ (|> firstBytePseudo) $ z80dstate
  in  grabStrings 0 dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

jumpTable :: Z80memory                   -- ^ Vector of bytes from which to extract some data
          -> Z80addr                     -- ^ Output's origin address
          -> Z80addr                     -- ^ Start address, relative to the origin, to start extracting bytes
          -> Z80disp                     -- ^ Number of bytes to extract
          -> Disassembly Z80DisasmState  -- ^ Current disassembly state
          -> Disassembly Z80DisasmState  -- ^ Resulting diassembly state
jumpTable mem origin sAddr nBytes dstate =
  let endAddr = sAddr + (fromIntegral nBytes)
      generateAddr addr theDState@(Z80Disassembly z80dstate)
        | addr < endAddr - 2  = generateAddr (addr + 2) $ operAddrPseudo (getAddress mem addr)
        | addr == endAddr - 2 = operAddrPseudo (getAddress mem addr)
        | otherwise           = z80disbytes mem origin addr (fromIntegral $ endAddr - addr) theDState
        where
          operAddrPseudo theAddr = Z80Disassembly $ disasmSeq %~ (|> DisasmPseudo (operAddr theAddr)) $ z80dstate
          operAddr theAddr = 
            let symTab = view symbolTab z80dstate
                oper   = if Map.member theAddr symTab then
                           SymAddr $ symTab Map.! theAddr
                         else
                           AbsAddr theAddr
                bytes  = DVU.slice (fromIntegral addr) 2 mem
            in  AddrWord (origin + addr) oper bytes
  in  generateAddr sAddr dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main = getArgs
       >>= (\args -> if length args == 1 then
                       let (imgName : _) = args
                       in  trs80Rom imgName
                     else
                       hPutStrLn stderr "Need only the ROM image name and only the ROM image name")
