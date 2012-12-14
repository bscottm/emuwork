module Guidance where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC

import Z80

-- | Disassembler "guidance": When to disassemble, when to dump bytes, ... basically guidance to the drive
-- the disassembly process (could be made more generic as part of a 'Machine' module.)

data Guidance where
  SetOrigin      :: Z80addr                     -- Assembly origin address
                 -> Guidance
  Equate         :: ByteString                  -- Symbolic name
                 -> Z80addr                     -- Address to associate with the symbolic name
                 -> Guidance
  Comment        :: ByteString                  -- Comment to output
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
          , Comment "Restart vector redirections. These are 'JP' instructions"
          , Equate "RST08VEC" 0x4000
          , Equate "RST10VEC" 0x4003
          , Equate "RST18VEC" 0x4006
          , Equate "RST20VEC" 0x4009
          , Equate "RST28VEC" 0x400c
          , Equate "RST30VEC" 0x400f
          , Equate "RST38VEC" 0x4012
          , Equate "KIDCB" 0x4015
          , Equate "CURSBLINK" 0x401c
          , Comment "Video device control block"
          , Equate "DODCB" 0x401d
          , Comment "Cursor position (2 bytes, LSB/MSB)"
          , Equate "CSRPOS"   0x4020
          , Comment "Line printer device control block"
          , Equate "PRDCB"    0x4025
          , Comment "Bad DCB vector, used by OUTDCB"
          , Equate "BADDCBVEC" 0x4033
          , Comment "Cassette port and line printer width control byte"
          , Comment "0=64 char, 8=32 char."
          , Equate "CASPLPRT" 0x403d
          , Comment "OSVER$: DOS version number"
          , Equate "DOSVER" 0x403e
          , Comment "25 millisecond clock count"
          , Equate "CLKTICK" 0x4040
          , Comment "TIME$: Time of day (seconds, minutes, hours)"
          , Equate "SYSTIME" 0x4041
          , Comment "DATE$: Day of year (year, month, day)"
          , Equate "SYSDATE" 0x4044
          , Comment "HIFH$: DOS highest unused RAM address"
          , Equate "HIFH" 0x404a
          , Comment "4080 - 41FF: Basic reserved area. L2INIRESRVD initializes this area"
          , Equate "BASICRESV" 0x4080
          , Equate "USRFNPTR" 0x408e
          , Comment "INKEY$ storage"
          , Equate "INKEYSTO" 0x4099
          , Comment "Error code for RESUME"
          , Equate "RESUMEERC" 0x409a
          , Comment "Printer carriage position"
          , Equate "PRCURPOS" 0x409b
          , Comment "Device type flag: -1 = tape, 0 = video, 1 = line printer"
          , Equate "DEVTYPEFLAG" 0x409c
          , Comment "PRINT# scratch space"
          , Equate "PRNUMWORK" 0x409d
          , Comment "Pointer to lowest address available for string storage"
          , Equate "STRINGLO" 0x40a0
          , Comment "BASIC program line number counter, current line being processed"
          , Equate "BASLINENO" 0x40a2
          , Comment "Start of BASIC program pointer, first byte where BASIC programs are stored"
          , Equate "BASPRGSTART" 0x40a4
          , Comment "Line cursor position"
          , Equate "LINECSRPOS" 0x40a6
          , Comment "Input buffer pointer"
          , Equate "INPBUFPTR" 0x40a7
          , Comment "RND seed"
          , Equate "RNDSEED" 0x40aa
          , Comment "NTF: Numberic Type Flag"
          , Comment "2: Integer"
          , Comment "3: String"
          , Comment "4: Single precision floating point"
          , Comment "8: Double precision floating point"
          , Comment "(see CPDE2HL)"
          , Equate "NTF" 0x40af
          , Comment "Top of memory/highest memory address available for string"
          , Comment "storage. Memory above this address pointer is 'reserved'."
          , Equate "MEMTOP" 0x40b1
          , Comment "STRWORKPTR: String work area pointer"
          , Equate "STRWORKPTR" 0x40b3
          , Comment "String work area (0x40b5 - 0x40d5"
          , Equate "STRWORKAREA" 0x40b5
          , Comment "Pointer to next byte of string storage"
          , Equate "NEXTSTRPTR" 0x40d6
          , Comment "Double precision accumulator: LSB, LSB, LSB, LSB, LSB, LSB, MSB, EXP"
          , Equate "DACC" 0x411d
          , Comment "Integer accumulator: LSB, MSB"
          , Comment "Single precision accumulator, LSB, LSB, MSB, EXP"
          , Equate "IACC" 0x4121
          , Comment "\"Hex\" accumulator: integer, single, double precision aligned at"
          , Comment "the same location."
          , Equate "HEXACC" 0x4127
          , Comment "TRSDOS command vector redirections. These are also 'JP' instructions"
          , Equate "DOSVECCMD" 0x4173
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Memory mapped I/O addresses"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Equate "LPPORT" 0x37e8
          , Equate "DSKCMDSTATUS" 0x37ec
          , Equate "DSKTRKSEL" 0x37ed
          , Equate "DSKSECSEL" 0x37ee
          , Equate "DSKDATA" 0x37ef
          , Equate "KBDLINE0" 0x3801
          , Equate "KBDLINEFNKEYS" 0x3840
          , Equate "KBDLINESHIFTS" 0x3880
          , Equate "VIDRAM" 0x3c00
          -- These are the locations where the BASIC CLOAD and SYSTEM "*"'s flicker.
          , Equate "VIDLINE0RIGHT1" 0x3c3e
          , Equate "VIDLINE0RIGHT2" 0x3c32
          , Comment "USR function pointer"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "TRS-80 Model I Level II ROM disassembly:"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment ""
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
          , nextSeg 0x020a 0x033a
          , Comment "This is an alternate entry point into CHARPRINT, which preserves"
          , Comment "DE. Note that CHARPRINT will load DE with the DO DCB."
          , nextSeg 0x033a 0x03c2
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Output character to a device, device control block is pointed to"
          , Comment "by DE, which is eventually transferred into IX. IX+1 -> L, IX + 2 -> H"
          , Comment "which is the device's output function. 0x3dd is also pushed onto"
          , Comment "the stack so that when the output function returns, registers are"
          , Comment "restored."
          , Comment BC.empty
          , Comment "Stack look like:"
          , Comment "BC"
          , Comment "HL"
          , Comment "IX"
          , Comment "DE"
          , Comment BC.empty
          , Comment "On entry, A has the character to be output. When control is transferred"
          , Comment "to the DCB's function, C has the character to be output."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment BC.empty
          , Comment " _______________________________________ "
          , Comment "| | | | | | | | |      |         |      |"
          , Comment "|    T Y P E    |VECTOR| SYSDATA | NAME |"
          , Comment "|_|_|_|_|_|_|_|_|______|_________|______|"
          , Comment " 7 6 5 4 3 2 1 0 15   0 23      0 15   0 "
          , Comment BC.empty
          , Comment "     The DCB follows a strict format that defines the utilization  of all four"
          , Comment "fields.  The  programmer  need  be  concerned only  with the TYPE  and  VECTOR"
          , Comment "fields.  The  system requires sole use of the SYSDATA field. It also maintains"
          , Comment "the NAME field thus usually necessitating no programmer intervention. The  DCB"
          , Comment "format must be followed in all Device Control Blocks established by  the user."
          , Comment "The following information provides specifications for each field of the DCB."
          , Comment BC.empty
          , Comment "TYPE Field - <Byte 0>"
          , Comment "---------------------"
          , Comment BC.empty
          , Comment "        Bit 7  => This bit specifies that the Control Block is actually a"
          , Comment "                  File Control Block (FCB) with the file in an OPEN"
          , Comment "                  condition. Since there is a great deal of similarity"
          , Comment "                  between DCBs and FCBs, and devices may be routed to"
          , Comment "                  files, tracing a path through a device chain may reveal"
          , Comment "                  a \"device\" with this bit set, indicating a routing to a"
          , Comment "                  file."
          , Comment BC.empty
          , Comment "        Bit 6  => This bit specifies that the DCB is associated with a"
          , Comment "                  FILTER module. The VECTOR field then contains the entry"
          , Comment "                  point of the filter. A filter initializer must set this"
          , Comment "                  bit when the module is assigned to the DCB."
          , Comment BC.empty
          , Comment "        Bit 5  => This bit specifies that the DCB (say device AA) is linked"
          , Comment "                  to another device associated with a DCB (say device BB)."
          , Comment "                  The VECTOR field of AA will point to a dummy LINK DCB (say"
          , Comment "                  device LK) which was established by the system when the"
          , Comment "                  LINK library command was invoked. The VECTOR field of LK"
          , Comment "                  then will point to the original VECTOR contents of AA"
          , Comment "                  while the SYSDATA field will contain a pointer to the BB"
          , Comment "                  DCB. A picture is said to be worth a thousand words. The"
          , Comment "                  device chain linkage will be illustrated later."
          , Comment BC.empty
          , Comment "        Bit 4  => This bit specifies that the device defined by the DCB is"
          , Comment "                  routed to another character-oriented device or file. The"
          , Comment "                  VECTOR field will either point to a DCB if the route"
          , Comment "                  destination is a device or it will contain a pointer to"
          , Comment "                  the file's FCB field contained in the route module"
          , Comment "                  established by the system's ROUTE library command."
          , Comment BC.empty
          , Comment "        Bit 3  => This bit specifies that the device defined by the DCB is"
          , Comment "                  a NIL device. Any output directed to the device will be"
          , Comment "                  discarded. Any input request will be satisfied with a"
          , Comment "                  ZERO return condition."
          , Comment BC.empty
          , Comment "        Bit 2  => This bit specifies that the device defined by the DCB is"
          , Comment "                  capable of handling requests generated by the @CTL Super-"
          , Comment "                  Visor Call."
          , Comment BC.empty
          , Comment "        Bit 1  => This bit specifies that the device defined by the DCB is"
          , Comment "                  capable of handling output requests which come from the"
          , Comment "                  @PUT SuperVisor Call."
          , Comment BC.empty
          , Comment "        Bit 0  => This bit specifies that the device defined by the DCB is"
          , Comment "                  capable of handling requests for input which come from"
          , Comment "                  the @GET SuperVisor Call."
          , Comment BC.empty
          , Comment "VECTOR Field - <Bytes 1 - 2>"
          , Comment "----------------------------"
          , Comment BC.empty
          , Comment "     This  field initially will contain the address of the driver routine that"
          , Comment "supports  the device  hardware  associated  with  the  DCB.  In  the  case  of"
          , Comment "programmer-installed  drivers, the  driver initialization  code must  load the"
          , Comment "driver's entry point  into  the VECTOR field of its  respective DCB. Likewise,"
          , Comment "when a  filter module is established  (via the SET library command), its entry"
          , Comment "point is placed into the VECTOR field. Once established by  either  the system"
          , Comment "or  the  driver/module  initialization code  to  point to  the  module's entry"
          , Comment "point, the VECTOR  field is then maintained by the system  to effect  routing,"
          , Comment "linking, and filtering."
          , Comment BC.empty
          , Comment "SYSDATA Field - <Bytes 3-5>"
          , Comment "---------------------------"
          , Comment BC.empty
          , Comment "     These three bytes are used by the system for routing and  linking and are"
          , Comment "unavailable for any other purpose."
          , Comment BC.empty
          , Comment "NAME Field - <Bytes 6 - 7>"
          , Comment "--------------------------"
          , Comment BC.empty
          , Comment "     Byte 6 of this  field contains the first character  and byte 7 the second"
          , Comment "character of the device specification name.  The system uses  the device  name"
          , Comment "field as a reference in searching the  Device Control Block tables. When a DCB"
          , Comment "is assigned  by the  system during a SET  or ROUTE command,  this  device name"
          , Comment "field  will be loaded by the system with the device specification name ppassed"
          , Comment "in  the  command  invocation. Programs requesting a spare DCB  via  the @GTDCB"
          , Comment " SuperVisor Call  (and  a binary ZERO name), are  responsible  for loading this"
          , Comment " name field."
          , Comment BC.empty
          , Comment "      If the device has been routed  to a file and a search of the device chain"
          , Comment " shows a  TYPE byte with bit-7 set,  then  the respective control  block  is an"
          , Comment " FCB.  In this case, byte 6  of the field will contain the  DRIVE number of the"
          , Comment " drive containing the file and  byte  7 will contain  the  Directory Entry Code"
          , Comment " (DEC) of the file."
          , Comment BC.empty
          , Comment BC.empty
          , nextSeg 0x03c2 0x03e3
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Read the keyboard's memory locations, determining if a key has"
          , Comment "been pressed. The scan starts as KBDLINE0, with the C register"
          , Comment "multiplied by 2 (3801, 3802, 3804, ...) via a rotate left until"
          , Comment "location 3880 is reached."
          , Comment BC.empty
          , Comment "Address    |0 0x01|1 0x02|2 0x04|3 0x08|4 0x10|5 0x20|6 0x40|7 0x80|"
          , Comment "-----------+------+------+------+------+------+------+------+-------"
          , Comment "3B01       |A     |B     |C     |D     |E     |F     |G     |      |"
          , Comment "3B02       |H     |I     |J     |K     |L     |M     |N     |O     |"
          , Comment "3B04       |P     |Q     |R     |S     |T     |U     |V     |W     |"
          , Comment "3B08       |X     |Y     |Z     |      |      |      |      |      |"
          , Comment "3B10       |0     |1     |2     |3     |4     |5     |6     |7     |"
          , Comment "3B20       |8     |9     |:     |;     |,     |-     |.     |/     |"
          , Comment "3B40       |ENTER |CLR   |UP-ARW|DN-ARW|L-ARW |R-ARW |SPACE |      |"
          , Comment "3B80       |RTSHFT|LTSHFT|      |      |      |      |      |      |"
          , Comment "-----------+------+------+------+------+------+------+------+-------"
          , Comment BC.empty
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x03e3 0x0458
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "DO (display output) DCB function"
          , Comment BC.empty
          , Comment "C has the character to be output on entry."
          , Comment "(IX + 3), (IX + 4): Current memory location in VIDRAM where the"
          , Comment "character is output (DCB SYSINFO bytes 1 and 2)."
          , Comment "(IX + 5): If non-zero, don't actually output the character, but"
          , Comment "do the rest of the character processing."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0458 0x058d
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "PR (line printer output) DCB function"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x058d 0x0674
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Initialize the restart vector table, located at RST08VEC"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0674 0x06d2
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Restart vector initialization table. Yes, this is code that is"
          , Comment "copied into RAM."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "0x4000 -> RST08 redirect"
          , nextSeg 0x06d2 0x06d5
          , Comment "0x4003 -> RST10 redirect"
          , nextSeg 0x06d5 0x06d8
          , Comment "0x4006 -> RST18 redirect"
          , nextSeg 0x06d8 0x06db
          , Comment "0x4009 -> RST20 redirect"
          , nextSeg 0x06db 0x06de
          , Comment "0x400c -> RST28 redirect"
          , nextSeg 0x06de 0x06e1
          , Comment "0x400f -> RST30 redirect"
          , nextSeg 0x06e1 0x06e4
          , Comment "0x4012 -> RST38 redirect"
          , nextSeg 0x06e4 0x06e7
          , Comment "0x4015: KI (keyboard) device control block"
          , GrabBytes romOrigin 0x06e7 1
          , JumpTable romOrigin 0x06e8 2
          , GrabBytes romOrigin 0x06ea 3
          , GrabAscii romOrigin 0x06ed 2
          , Comment "0x401d: DO (display output) device control block"
          , GrabBytes romOrigin 0x06ef 1
          , JumpTable romOrigin 0x06f0 2
          , GrabBytes romOrigin 0x06f2 3
          , GrabAscii romOrigin 0x06f5 2
          , Comment "0x4025: PR (line printer output) device control block"
          , GrabBytes romOrigin 0x06f7 1
          , JumpTable romOrigin 0x06f8 2
          , GrabBytes romOrigin 0x06fa 3
          , GrabAscii romOrigin 0x06fd 2
          , Comment "0x402d:"
          , nextSeg 0x06ff 0x0705
          , Comment "0x4033: OUTDCB jumps here if the requested device type bit doesn't match"
          , nextSeg 0x0705 0x0708
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "End of the restart vector initialization table."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0708 0x09d2
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Copy bytes pointed to by HL to the buffer pointed to by DE"
          , Comment "This just exchanges DE and HL, falls through to CPDE2HL"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x09d2 0x09d3
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Copy the bytes pointed to by DE to the buffer pointed to by HL"
          , Comment "A, B, DE and HL are not preserved. NTF (numeric type flag)"
          , Comment "indicates the number of bytes to copy."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x09d3 0x137c
          , Comment "0.0 floating point constant"
          , GrabBytes romOrigin 0x137c 4
          , Comment "0.5 floating point constant"
          , GrabBytes romOrigin 0x1380 4
          , Comment "?? floating point constant"
          , GrabBytes romOrigin 0x1384 4
          , nextSeg 0x1388 0x158b
          , Comment "These four bytes are a floating point constant?"
          , GrabBytes romOrigin 0x158b 4
          , nextSeg 0x158f 0x1650
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "BASIC verb table -- first character has high bit set"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , HighBitTable romOrigin 0x1650 (0x1820 - 0x1650)
          , JumpTable romOrigin 0x1820 (0x18c9 - 0x1820)
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "BASIC error codes:"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
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
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "BASIC reserved data table."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , GrabBytes romOrigin 0x18f7 39
          , GrabAscii romOrigin 0x191e 5
          , GrabBytes romOrigin 0x1923 1
          , GrabAscii romOrigin 0x1924 4
          , GrabBytes romOrigin 0x1928 1
          , GrabAscii romOrigin 0x1929 5
          , GrabBytes romOrigin 0x192e 2
          , GrabAscii romOrigin 0x1930 5
          , GrabBytes romOrigin 0x1935 1
          , nextSeg 0x1936 0x1c96
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Compare A against character following the RST 08 instruction."
          , Comment "Return address is pointed to by SP, which is transferred into HL"
          , Comment "The return address is incremented so that the RET returns to the"
          , Comment "instruction following the character."
          , Comment BC.empty
          , Comment "It's a neat hack."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x1c96 0x25d9
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Test the numberic type flag (NTF)"
          , Comment "Z: String"
          , Comment "M: Integer"
          , Comment "P, C: Single precision"
          , Comment "P, NC, Double precision"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x25d9 0x2fff
          ]
  where
    nextSeg sAddr eAddr = DoDisasm romOrigin sAddr (fromIntegral (eAddr - sAddr) :: Z80disp)
