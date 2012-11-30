{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Guidance where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC

import Z80

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
          , AddAddrEquate "CURSBLINK" 0x401c
          , AddLineComment "Cursor position (2 bytes, LSB/MSB)"
          , AddAddrEquate "CSRPOS"   0x4020
          , AddLineComment "Cassette port and line printer width control byte"
          , AddAddrEquate "CASPLPRT" 0x403d
          , AddLineComment "Line printer device control block"
          , AddAddrEquate "LPDCB"    0x4025
          , AddLineComment "4080 - 41FF: Basic reserved area. L2INIRESRVD initializes this area"
          , AddAddrEquate "BASICRESV" 0x4080
          , AddLineComment "Memory mapped I/O addresses"
          , AddAddrEquate "LPPORT" 0x37e8
          , AddAddrEquate "DSKCMDSTATUS" 0x37ec
          , AddAddrEquate "DSKTRKSEL" 0x37ed
          , AddAddrEquate "DSKSECSEL" 0x37ee
          , AddAddrEquate "DSKDATA" 0x37ef
          , AddAddrEquate "KBDLINE0" 0x3801
          , AddAddrEquate "KBDLINEFNKEYS" 0x3840
          , AddAddrEquate "KBDLINESHIFTS" 0x3880
          , AddAddrEquate "VIDRAM" 0x3c00
	  , AddLineComment "USR function pointer"
	  , AddAddrEquate "USRFNPTR" 0x408e
          , AddLineComment "INKEY$ storage"
          , AddAddrEquate "INKEYSTO" 0x4099
          , AddLineComment "NTF: Numberic Type Flag"
          , AddAddrEquate "NTF" 0x40af
          , AddLineComment "Double precision accumulator: LSB, LSB, LSB, LSB, LSB, LSB, MSB, EXP"
          , AddAddrEquate "DACC" 0x411d
          , AddLineComment "Integer accumulator: LSB, MSB"
          , AddLineComment "Single precision accumulator, LSB, LSB, MSB, EXP"
          , AddAddrEquate "IACC" 0x4121
          , AddLineComment "\"Hex\" accumulator: integer, single, double precision aligned at"
          , AddLineComment "the same location."
          , AddAddrEquate "HEXACC" 0x4127
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
          , nextSeg 0x020a 0x03e3
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "Read the keyboard's memory locations, determining if a key has"
          , AddLineComment "been pressed. The scan starts as KBDLINE0, with the C register"
          , AddLineComment "multiplied by 2 (3801, 3802, 3804, ...) via a rotate left until"
          , AddLineComment "location 3880 is reached."
          , AddLineComment BC.empty
          , AddLineComment "Address    |0 0x01|1 0x02|2 0x04|3 0x08|4 0x10|5 0x20|6 0x40|7 0x80|"
          , AddLineComment "-----------+------+------+------+------+------+------+------+-------"
          , AddLineComment "3B01       |A     |B     |C     |D     |E     |F     |G     |      |"
          , AddLineComment "3B02       |H     |I     |J     |K     |L     |M     |N     |O     |"
          , AddLineComment "3B04       |P     |Q     |R     |S     |T     |U     |V     |W     |"
          , AddLineComment "3B08       |X     |Y     |Z     |      |      |      |      |      |"
          , AddLineComment "3B10       |0     |1     |2     |3     |4     |5     |6     |7     |"
          , AddLineComment "3B20       |8     |9     |:     |;     |,     |-     |.     |/     |"
          , AddLineComment "3B40       |ENTER |CLR   |UP-ARW|DN-ARW|L-ARW |R-ARW |SPACE |      |"
          , AddLineComment "3B80       |RTSHFT|LTSHFT|      |      |      |      |      |      |"
          , AddLineComment "-----------+------+------+------+------+------+------+------+-------"
          , AddLineComment BC.empty
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x03e3 0x0674
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "Initialize the restart vector table, located at RST08VEC"
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
          , nextSeg 0x0708 0x137c
          , AddLineComment "0.0 floating point constant"
          , GrabBytes romOrigin 0x137c 4
          , AddLineComment "0.5 floating point constant"
          , GrabBytes romOrigin 0x1380 4
          , nextSeg 0x1384 0x158b
          , AddLineComment "These four bytes are a floating point constant?"
          , GrabBytes romOrigin 0x158b 4
          , nextSeg 0x158f 0x1650
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
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "BASIC reserved data table."
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , GrabBytes romOrigin 0x18f7 39
          , GrabAscii romOrigin 0x191e 5
          , GrabBytes romOrigin 0x1923 1
          , GrabAscii romOrigin 0x1924 4
          , GrabBytes romOrigin 0x1928 1
          , GrabAscii romOrigin 0x1929 5
          , GrabBytes romOrigin 0x192e 2
          , GrabAscii romOrigin 0x1930 5
          , GrabBytes romOrigin 0x1935 1
          , nextSeg 0x1935 0x1c96
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "Compare A against character following the RST 08 instruction."
          , AddLineComment "Return address is pointed to by SP, which is transferred into HL"
          , AddLineComment "The return address is incremented so that the RET returns to the"
          , AddLineComment "instruction following the character."
          , AddLineComment BC.empty
          , AddLineComment "It's a neat hack."
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x1c96 0x25d9
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , AddLineComment "Test the numberic type flag (NTF)"
          , AddLineComment "Z: String"
          , AddLineComment "M: Integer"
          , AddLineComment "P, C: Single precision"
          , AddLineComment "P, NC, Double precision"
          , AddLineComment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x25d9 0x2fff
          ]
  where
    nextSeg sAddr eAddr = DoDisasm romOrigin sAddr (fromIntegral (eAddr - sAddr) :: Z80disp)
