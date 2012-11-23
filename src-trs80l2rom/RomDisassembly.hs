{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | The venerable TRS-80 Level II ROM disassembly module.
--
-- This module is merely a driver to disassemble the TRS-80 Level II ROM, complete with annotations
module Main where

import System.IO (stderr, hPutStrLn)
import System.Environment
import qualified Data.List as DL
import Data.Label
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Reader.RawFormat
import Machine.DisassemblerTypes
import Z80.Processor
import Z80.Disassembler
import Z80.DisasmOutput

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- Template Haskell hair for lenses
mkLabel ''Disassembly

-- | "Driver" data: When to disassemble, when to dump bytes, ... basically guidance to the drive
-- the disassembly process (could be made more generic as part of a 'Machine' module.)

data Guidance where
  DoDisasm   :: Z80addr                           -- Origin
             -> Z80addr                           -- Start of disassembly
             -> Z80disp                           -- Number of bytes to disassemble
             -> Guidance
  GrabBytes  :: Z80addr                          -- Disassembly origin
             -> Z80addr                          -- Start of range
             -> Z80disp                          -- Number of bytes to grab
             -> Guidance
  GrabAsciiZ :: Z80addr                         -- Disassembly origin
             -> Z80addr                         -- Start address to start grabbing 0-terminated ASCII string
             -> Guidance
  GrabAscii  :: Z80addr                          -- Disassembly origin
             -> Z80addr                          -- Start of range
             -> Z80disp                          -- Number of bytes to grab
             -> Guidance

-- | Disassemble the TRS-80 ROM (with annotations, known symbols, ...)
trs80Rom :: String 
         -> IO ()
trs80Rom imgName = readRawWord8Vector imgName
                   >>= (\ img -> BS.putStrLn $ outputDisassembly $ collectRom img initialDisassembly actions)
  where
    -- 
    initialDisassembly = set symbolTab knownSymbols $ set addrInDisasmRange trs80RomRange mkInitialDisassembly
    -- The ROM's origin (constant)
    romOrigin = 0x0000 :: Z80addr
    -- Drive the disassembly process
    actions = [ DoDisasm romOrigin 0x0000 0x0050
              , GrabBytes romOrigin 0x0050 0x0010
              , DoDisasm romOrigin 0x0060 (0x0105 - 0x0060)
              , GrabAscii romOrigin 0x0105 (0x0110 - 0x0105)
              , GrabBytes romOrigin 0x0110 1
              , GrabAscii romOrigin 0x0111 (0x012b - 0x0111)
              , GrabBytes romOrigin 0x012b 2
              , DoDisasm romOrigin 0x012d (0x1000 - 0x012d)
              ]
    lastAddr = 0x1000
    -- ROM range for 'addrInDisasmRange' predicate
    trs80RomRange addr = addr >= romOrigin && addr <= lastAddr

knownSymbols :: Map Z80addr ByteString
knownSymbols   = Map.fromList [ (0x0000, "RST00")
                              , (0x0008, "RST08")
                              , (0x0010, "RST10")
                              , (0x0018, "RST18")
                              , (0x0020, "RST20")
                              , (0x0028, "RST38")
                              , (0x002b, "KBDSCN")
                              , (0x0030, "RST30")
                              , (0x0033, "CHARPRINT")
                              , (0x0038, "RST38")
                              , (0x003b, "LPRINTCHAR")
                              , (0x0040, "KBLINE")
                              , (0x0049, "KBWAIT")
                              , (0x0050, "CTRLCHARS")
                              , (0x0060, "DELAY")
                              , (0x0066, "NMI_RESET")
                              , (0x0069, "HAVEDISK")
                              , (0x0075, "L2INIRESRVD")
                              , (0x008b, "SETIOB")
                              , (0x0091, "L2INIT2")
                              , (0x0105, "MEMSIZE")
                              , (0x0111, "L2LOGO")
                              , (0x012d, "L3ERROR")
                              , (0x0132, "CMDPOINT")
                              , (0x0135, "CMDSET")
                              , (0x0138, "CMDRESET")
                              , (0x0150, "GRSTART")
                              , (0x01c9, "CLS")
                              , (0x01d3, "GETRANDOM")
                              , (0x01d9, "CASSPULSE1")
                              , (0x01df, "CASSDLY1")
                              , (0x01e3, "CASSPULSE2")
                              , (0x01e9, "CASSDLY2")
                              , (0x01ed, "CASSPULSE3")
                              , (0x01f3, "CASSDLY3")
                              , (0x01f8, "CASSOFF")
                              ]

collectRom :: Z80memory
           -> Z80Disassembly
           -> [Guidance]
           -> Z80Disassembly
collectRom img dstate actions = DL.foldl (doAction img) dstate actions

doAction :: Z80memory
         -> Z80Disassembly
         -> Guidance
         -> Z80Disassembly
doAction img dstate (DoDisasm origin sAddr nBytes)   = z80disassembler img origin sAddr nBytes dstate
doAction img dstate (GrabBytes origin sAddr nBytes)  = z80disbytes img origin sAddr nBytes dstate
doAction img dstate (GrabAsciiZ origin sAddr)        = z80disasciiz img origin sAddr dstate
doAction img dstate (GrabAscii origin sAddr nBytes)  = z80disascii img origin sAddr nBytes dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main = getArgs
       >>= (\args -> if length args == 1 then
                       let (imgName : _) = args
                       in  trs80Rom imgName
                     else
                       hPutStrLn stderr "Need only the ROM image name and only the ROM image name")
