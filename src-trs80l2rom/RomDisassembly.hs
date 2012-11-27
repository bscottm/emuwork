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
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as DVU
import Data.Bits
import Data.Char

-- import Language.Haskell.Pretty

import Reader.RawFormat
import Machine.DisassemblerTypes
import Z80.Processor
import Z80.InstructionSet (Z80memory, getAddress, OperAddr(..))
import Z80.Disassembler
import Z80.DisasmOutput

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- Template Haskell hair for lenses
mkLabel ''Disassembly

-- | Disassembler "guidance": When to disassemble, when to dump bytes, ... basically guidance to the drive
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
  HighBitTable   :: Z80addr                     -- Disassembly origin
                 -> Z80addr                     -- Start of table
                 -> Z80disp                     -- Table length
                 -> Guidance
  JumpTable      :: Z80addr                     -- Diassembly origin
                 -> Z80addr                     -- Jump table start
                 -> Z80disp                     -- Jump table length
                 -> Guidance
  deriving (Show)

-- | Disassemble the TRS-80 ROM (with annotations, known symbols, ...)
trs80Rom :: String 
         -> IO ()
trs80Rom imgName = readRawWord8Vector imgName
                   >>= (\ img -> BS.putStrLn $ outputDisassembly $ collectRom img initialDisassembly actions)
                   -- >> putStrLn (prettyPrint actions)
  where
    -- Initial disassembly state: known symbols and disassembly address range predicate
    initialDisassembly = set symbolTab knownSymbols $ set addrInDisasmRange trs80RomRange mkInitialDisassembly
    lastAddr = 0x2fff
    -- ROM range for 'addrInDisasmRange' predicate
    trs80RomRange addr = addr >= romOrigin && addr <= lastAddr

-- The ROM's origin (constant)
romOrigin :: Z80addr
romOrigin = 0x0000

-- Drive the disassembly process
actions :: [Guidance]
actions = [ SetOrigin romOrigin
          -- Jump vector equates
          , AddLineComment "Restart vector redirections. These are 'JP' instructions"
          , AddAddrEquate "RST08VEC" 0x4000
          , AddAddrEquate "RST10VEC" 0x4003
          , AddAddrEquate "RST18VEC" 0x4006
          , AddAddrEquate "RST20VEC" 0x4009
          , AddAddrEquate "RST28VEC" 0x400c
          , AddAddrEquate "RST30VEC" 0x400f
          , AddAddrEquate "RST38VEC" 0x4012
          , AddLineComment "Memory mapped I/O addresses"
          , AddAddrEquate "LPPORT" 0x37e8
          , AddAddrEquate "DSKCMDSTATUS" 0x37ec
          , AddAddrEquate "DSKTRKSEL" 0x37ed
          , AddAddrEquate "DSKSECSEL" 0x37ee
          , AddAddrEquate "DSKDATA" 0x37ef
          , AddAddrEquate "VIDRAM" 0x3c00
          , AddLineComment "INKEY$ storage"
          , AddAddrEquate "INKEYSTO" 0x4099
          , AddLineComment "NTF: Numberic Type Flag"
          , AddAddrEquate "NTF" 0x40af
          , AddLineComment "Double precision accumulator"
          , AddAddrEquate "DACC" 0x411d
          , AddLineComment "Integer accumulator: LSB, MSB; also single precision accumulator, LSB, LSB, MSB, EXP"
          , AddAddrEquate "IACC" 0x4121
          , AddLineComment "TRSDOS command vector redirections. These are also 'JP' instructions"
          , AddAddrEquate "DOSVECCMD" 0x4173
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
          , GrabBytes romOrigin 0x0506 0x3a
          , nextSeg 0x0541 0x0674
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "Initialize the restart vector table, located at RST00VEC"
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0674 0x06d2
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "Restart vector initialization table. Yes, this is code that is"
          , AddLineComment "copied into RAM."
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x06d2 0x0708
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "End of the restart vector initialization table."
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0708 0x1650
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "BASIC verb table -- first character has high bit set"
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , HighBitTable romOrigin 0x1650 (0x181f - 0x1650)
          , JumpTable romOrigin 0x1820 (0x18c9 - 0x1820)
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "BASIC error codes:"
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
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
          , nextSeg 0x18f7 0x191d
          , GrabAscii romOrigin 0x191d 6
          , GrabBytes romOrigin 0x1923 1
          , GrabAscii romOrigin 0x1924 4
          , GrabBytes romOrigin 0x1928 1
          , GrabAscii romOrigin 0x1929 5
          , GrabBytes romOrigin 0x192e 2
          , GrabAscii romOrigin 0x1930 5
          , GrabBytes romOrigin 0x1935 1
          , nextSeg 0x1935 0x2fff
          ]
  where
    nextSeg sAddr eAddr = DoDisasm romOrigin sAddr (fromIntegral (eAddr - sAddr) :: Z80disp)

knownSymbols :: Map Z80addr ByteString
knownSymbols   = Map.fromList [ (0x0000, "RST00")
                              , (0x0008, "RST08")
                              , (0x0010, "RST10")
                              , (0x0018, "RST18")
                              , (0x0020, "RST20")
                              , (0x0028, "RST28")
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
                              , (0x0135, "BASSET")
                              , (0x0138, "BASRESET")
                              , (0x0150, "GRSTART")
                              , (0x01c9, "CLS")
                              , (0x01d3, "BASRANDOM")
                              , (0x01d9, "CASSPULSE1")
                              , (0x01df, "CASSDLY1")
                              , (0x01e3, "CASSPULSE2")
                              , (0x01e9, "CASSDLY2")
                              , (0x01ed, "CASSPULSE3")
                              , (0x01f3, "CASSDLY3")
                              , (0x01f8, "CASSOFF")
                              , (0x0506, "CURCON")
                              , (0x0674, "INITRSTVECS")
                              , (0x06d2, "RSTVECTABLE")
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
                              , (0x18f5, "ERRL3DOS")
                              , (0x1929, "READYPMT")
                              , (0x1930, "BREAKMSG")
                              , (0x191d, "ERORORIN")
                              , (0x1c90, "CMPDEHL")
                              , (0x1c96, "DEFRST08")
                              , (0x1ca1, "BASFOR")
                              , (0x1d78, "DEFRST10")
                              , (0x1d91, "BASRESTORE")
                              , (0x1dae, "BASEND")
                              , (0x1da9, "BASSTOP")
                              , (0x1df7, "BASTRON")
                              , (0x1df8, "BASTROFF")
                              , (0x1eb1, "BASGOSUB")
                              , (0x1ede, "BASRETURN")
                              , (0x1ec2, "BASGOTO")
                              , (0x1ea3, "BASRUN")
                              , (0x1f05, "BASDATA")
                              , (0x1f07, "BASREM")
                              , (0x1f21, "BASLET")
                              , (0x2039, "BASIF")
                              , (0x219a, "BASINPUT")
                              , (0x21ef, "BASREAD")
                              , (0x22b6, "BASNEXT")
                              , (0x25d9, "DEFRST18")
                              , (0x2608, "BASDIM")
                              ]

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

collectRom :: Z80memory
           -> Z80Disassembly
           -> [Guidance]
           -> Z80Disassembly
collectRom img dstate theActions = DL.foldl (doAction img) dstate theActions

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

doAction :: Z80memory
         -> Z80Disassembly
         -> Guidance
         -> Z80Disassembly
doAction _img dstate (SetOrigin origin)               = modify disasmSeq (|> DisasmPseudo (DisOrigin origin)) dstate
doAction _img dstate (AddAddrEquate label addr)       = modify symbolTab (Map.insert addr label) $
                                                        modify disasmSeq (|> DisasmPseudo (AddrEquate label addr)) dstate
doAction _img dstate (AddLineComment comment)         = modify disasmSeq (|> DisasmPseudo (LineComment comment)) dstate
doAction img dstate (DoDisasm origin sAddr nBytes)    = z80disassembler img origin sAddr nBytes dstate
doAction img dstate (GrabBytes origin sAddr nBytes)   = z80disbytes img origin sAddr nBytes dstate
doAction img dstate (GrabAsciiZ origin sAddr)         = z80disasciiz img origin sAddr dstate
doAction img dstate (GrabAscii origin sAddr nBytes)   = z80disascii img origin sAddr nBytes dstate
doAction img dstate (HighBitTable origin addr nBytes) = highbitCharTable img origin addr nBytes dstate
doAction img dstate (JumpTable origin addr nBytes)    = jumpTable img origin addr nBytes dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | The TRS-80's BASIC has a table of keywords, where the first letter of the keyword has the
-- high bit on. Scan through and generate all of the pseudo-operations for this table, within
-- the specified memory range.
highbitCharTable :: Z80memory                   -- ^ Vector of bytes from which to extract some data
                 -> Z80addr                     -- ^ Output's origin address
                 -> Z80addr                     -- ^ Start address, relative to the origin, to start extracting bytes
                 -> Z80disp                     -- ^ Number of bytes to extract
                 -> Z80Disassembly              -- ^ Current disassembly state
                 -> Z80Disassembly              -- ^ Resulting diassembly state
highbitCharTable mem origin sAddr nBytes dstate =
  let sAddr'   = fromIntegral sAddr
      nBytes'  = fromIntegral nBytes
      -- Look for the high bit characters within the address range, then convert back to addresses
      byteidxs = DVU.map (\x -> x + sAddr') $ DVU.findIndices (\c -> c >= 0x80) $ DVU.slice sAddr' nBytes' mem
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
      grabString memidx memidx' theDState =
        let theChar = mem ! memidx
            theChar' = fromIntegral theChar :: Int
            firstByte = BS.concat [ "'"
                                  , BS.singleton (chr (theChar' .&. 0x7f))
                                  , "' .|. 80H"
                                  ]
            firstBytePseudo = DisasmPseudo $ ByteExpression (origin + (fromIntegral memidx)) firstByte theChar
            theString = DisasmPseudo $ Ascii (origin + (fromIntegral memidx) + 1)
                                             (DVU.slice (memidx + 1) (memidx' - memidx - 1) mem)
        in  if (memidx + 1) /= memidx' then
              modify disasmSeq (|> theString) $
              modify disasmSeq (|> firstBytePseudo) theDState
            else
              modify disasmSeq (|> firstBytePseudo) theDState
  in  grabStrings 0 dstate

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

jumpTable :: Z80memory                   -- ^ Vector of bytes from which to extract some data
          -> Z80addr                     -- ^ Output's origin address
          -> Z80addr                     -- ^ Start address, relative to the origin, to start extracting bytes
          -> Z80disp                     -- ^ Number of bytes to extract
          -> Z80Disassembly              -- ^ Current disassembly state
          -> Z80Disassembly              -- ^ Resulting diassembly state
jumpTable mem origin sAddr nBytes dstate =
  let endAddr = sAddr + (fromIntegral nBytes)
      generateAddr addr theDState
        | addr < endAddr - 2  = generateAddr (addr + 2) $ operAddrPseudo (getAddress mem addr)
        | addr == endAddr - 2 = operAddrPseudo (getAddress mem addr)
        | otherwise           = z80disbytes mem origin addr (fromIntegral $ endAddr - addr) theDState
        where
          operAddrPseudo theAddr = modify disasmSeq (|> DisasmPseudo (operAddr theAddr)) theDState
          operAddr theAddr = 
            let symTab = get symbolTab theDState
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
