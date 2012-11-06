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
import Data.Sequence ((|>))

import Language.Haskell.Pretty

import Reader.RawFormat
import Machine.DisassemblerTypes
import Z80.Processor
import Z80.InstructionSet (Z80memory)
import Z80.Disassembler
import Z80.DisasmOutput

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- Template Haskell hair for lenses
mkLabel ''Disassembly

-- | "Driver" data: When to disassemble, when to dump bytes, ... basically guidance to the drive
-- the disassembly process (could be made more generic as part of a 'Machine' module.)

data Guidance where
  SetOrigin      :: Z80addr                     -- Assembly origin address
                 -> Guidance
  AddAddrEquate  :: ByteString                  -- Symbolic name
                 -> Z80addr                     -- Address to associate with the symbolic name
                 -> Guidance
  AddLineComment :: ByteString                  -- Comment to output
                 -> Guidance
  DoDisasm       :: Z80addr                     -- Origin
                 -> Z80addr                     -- Start of disassembly
                 -> Z80disp                     -- Number of bytes to disassemble
                 -> Guidance
  GrabBytes      :: Z80addr                     -- Disassembly origin
                 -> Z80addr                     -- Start of range
                 -> Z80disp                     -- Number of bytes to grab
                 -> Guidance
  GrabAsciiZ     :: Z80addr                     -- Disassembly origin
                 -> Z80addr                     -- Start address to start grabbing 0-terminated ASCII string
                 -> Guidance
  GrabAscii      :: Z80addr                     -- Disassembly origin
                 -> Z80addr                     -- Start of range
                 -> Z80disp                     -- Number of bytes to grab
                 -> Guidance
  deriving (Show)

-- | Disassemble the TRS-80 ROM (with annotations, known symbols, ...)
trs80Rom :: String 
         -> IO ()
trs80Rom imgName = readRawWord8Vector imgName
                   >>= (\ img -> BS.putStrLn $ outputDisassembly $ collectRom img initialDisassembly actions)
                   -- >> putStrLn (prettyPrint actions)
  where
    -- 
    initialDisassembly = set symbolTab knownSymbols $ set addrInDisasmRange trs80RomRange mkInitialDisassembly
    -- The ROM's origin (constant)
    romOrigin = 0x0000 :: Z80addr
    -- Drive the disassembly process
    actions = [ SetOrigin romOrigin
              -- Memory mapped I/O equates
              , AddAddrEquate "LPPORT" 0x37e8
              , AddAddrEquate "DSKCMDSTATUS" 0x37ec
              , AddAddrEquate "DSKTRKSEL" 0x37ed
              , AddAddrEquate "DSKSECSEL" 0x37ee
              , AddAddrEquate "DSKDATA" 0x37ef
              , AddAddrEquate "VIDRAM" 0x3c00
	      , AddAddrEquate "INKEYSTO" 0x4099
              , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
              , AddLineComment "TRS-80 Model I Level II ROM disassembly:"
              , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
              , AddLineComment ""
              , DoDisasm romOrigin 0x0000 0x0050
              , GrabBytes romOrigin 0x0050 0x0010
              , nextSeg 0x0060 0x0105
              , GrabAscii romOrigin 0x0105 (0x0110 - 0x0105)
              , GrabBytes romOrigin 0x0110 1
              , GrabAscii romOrigin 0x0111 (0x012b - 0x0111)
              , GrabBytes romOrigin 0x012b 2
              , nextSeg 0x012d 0x013c
              , GrabAscii romOrigin 0x013c 1
              , nextSeg 0x13d 0x0147
              , GrabAscii romOrigin 0x0147 1
              , nextSeg 0x0148 0x018d
              , GrabAscii romOrigin 0x018d 1
              , nextSeg 0x018e 0x0209
              , GrabAscii romOrigin 0x0209 1
              , nextSeg 0x020a 0x0506
              , GrabBytes romOrigin 0x0506 58
              , DoDisasm romOrigin 0x0541 (0x18c9 - 0x0506)
              , GrabAscii romOrigin 0x18c9 2
              , GrabAscii romOrigin 0x18cb 2
              , GrabAscii romOrigin 0x18cd 2
              , GrabAscii romOrigin 0x18cf 2
              , GrabAscii romOrigin 0x18d1 2
              , GrabAscii romOrigin 0x18d3 2
              , GrabAscii romOrigin 0x18d5 2
              , GrabAscii romOrigin 0x18d7 2
              , GrabAscii romOrigin 0x18d9 2
              , GrabAscii romOrigin 0x18db 2
              , GrabAscii romOrigin 0x18dd 2
              , GrabAscii romOrigin 0x18df 2
              , GrabAscii romOrigin 0x18e1 2
              , GrabAscii romOrigin 0x18e3 2
              , GrabAscii romOrigin 0x18e5 2
              , GrabAscii romOrigin 0x18e7 2
              , GrabAscii romOrigin 0x18e9 2
              , GrabAscii romOrigin 0x18eb 2
              , GrabAscii romOrigin 0x18ed 2
              , GrabAscii romOrigin 0x18ef 2
              , GrabAscii romOrigin 0x18f1 2
              , GrabAscii romOrigin 0x18f3 2
              , GrabAscii romOrigin 0x18f5 2
              , DoDisasm  romOrigin 0x18f7 (0x2fff - 0x18f7)
              ]

    nextSeg sAddr eAddr = DoDisasm romOrigin sAddr (fromIntegral (eAddr - sAddr) :: Z80disp)

    lastAddr = 0x2fff

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
                              , (0x0506, "CURCON")
                              -- BASIC trancendental functions
                              , (0x1541, "BASCOS")
                              , (0x1547, "BASSIN")
                              , (0x15a8, "BASTAN")
                              , (0x15bd, "BASATN")
                              -- BASIC's error strings
                              , (0x18c9, "ERRNOFOR")
                              , (0x18cb, "ERRSYNTAX")
                              , (0x18cd, "ERRRANGE")
                              , (0x18cf, "ERROUTOFDATA")
                              , (0x18d1, "ERRBADFUNC")
                              , (0x18d3, "ERROVERFLOW")
                              , (0x18d5, "ERROUTOFMEM")
                              , (0x18d7, "ERRUNDEFLINE")
                              , (0x18d9, "ERRBADSUBSCR")
                              , (0x18db, "ERRBADDIM")
                              , (0x18dd, "ERRDIV0")
                              , (0x18df, "ERRILLEGALDIRECT")
                              , (0x18e1, "ERRTYPEMISMATCH")
                              , (0x18e3, "ERRNOSTRSPACE")
                              , (0x18e5, "ERRLONGSTRING")
                              , (0x18e7, "ERRCOMPLEXSTR")
                              , (0x18e9, "ERRNOCONT")
                              , (0x18eb, "ERRNORESUME")
                              , (0x18ed, "ERRRESUMENOERR")
                              , (0x18ef, "ERRUNPRINTERR")
                              , (0x18f1, "ERRMISSOPERAND")
                              , (0x18f3, "ERRFILEDATA")
                              , (0x18f5, "ERRLEVEL3DOS")
                              -- Sundry commands
                              , (0x1df7, "BASTRON")
                              , (0x1df8, "BASTROFF")
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
doAction _img dstate (SetOrigin origin)              = modify disasmSeq (|> DisasmPseudo (DisOrigin origin)) dstate
doAction _img dstate (AddAddrEquate label addr)      = modify symbolTab (Map.insert addr label) $
                                                       modify disasmSeq (|> DisasmPseudo (AddrEquate label addr)) dstate
doAction _img dstate (AddLineComment comment)        = modify disasmSeq (|> DisasmPseudo (LineComment comment)) dstate
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
