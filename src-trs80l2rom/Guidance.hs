module Guidance where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC

import Z80

-- | Disassembler "guidance": When to disassemble, when to dump bytes, ... basically guidance to the drive
-- the disassembly process (could be made more generic as part of a 'Machine' module.)

data Guidance where
  SetOrigin      :: Z80addr                     -- Assembly origin address
                 -> Guidance
  SymEquate      :: ByteString                  -- Symbolic name
                 -> Z80addr                     -- Address to associate with the symbolic name
                 -> Guidance
  Comment        :: ByteString                  -- Comment to output
                 -> Guidance
  DoDisasm       :: Z80addr                     -- Start of disassembly
                 -> Z80disp                     -- Number of bytes to disassemble
                 -> Guidance
  GrabBytes      :: Z80addr                     -- Start of range
                 -> Z80disp                     -- Number of bytes to grab
                 -> Guidance
  GrabAsciiZ     :: Z80addr                     -- Start address to start grabbing 0-terminated ASCII string
                 -> Guidance
  GrabAscii      :: Z80addr                     -- Start of range
                 -> Z80disp                     -- Number of bytes to grab
                 -> Guidance
  HighBitTable   :: Z80addr                     -- Start of table
                 -> Z80disp                     -- Table length
                 -> Guidance
  JumpTable      :: Z80addr                     -- Jump table start
                 -> Z80disp                     -- Jump table length
                 -> Guidance
  deriving (Show)

-- Drive the disassembly process
actions :: [Guidance]
actions = [ SetOrigin 0x0000
          -- Jump vector equates
          , Comment "Restart vector redirections. These are 'JP' instructions"
          , SymEquate "RST08VEC" 0x4000
          , SymEquate "RST10VEC" 0x4003
          , SymEquate "RST18VEC" 0x4006
          , SymEquate "RST20VEC" 0x4009
          , SymEquate "RST28VEC" 0x400c
          , SymEquate "RST30VEC" 0x400f
          , SymEquate "RST38VEC" 0x4012
          , SymEquate "KIDCB" 0x4015
          , SymEquate "CURSBLINK" 0x401c
          , Comment "Video device control block"
          , SymEquate "DODCB" 0x401d
          , Comment "Cursor position (2 bytes, LSB/MSB)"
          , SymEquate "CSRPOS"   0x4020
          , Comment "Line printer device control block"
          , SymEquate "PRDCB"    0x4025
          , Comment "Bad DCB vector, used by OUTDCB"
          , SymEquate "BADDCBVEC" 0x4033
          , Comment "Cassette port and line printer width control byte"
          , Comment "0=64 char, 8=32 char."
          , SymEquate "CASPLPRT" 0x403d
          , Comment "OSVER$: DOS version number"
          , SymEquate "DOSVER" 0x403e
          , Comment "25 millisecond clock count"
          , SymEquate "CLKTICK" 0x4040
          , Comment "TIME$: Time of day (seconds, minutes, hours)"
          , SymEquate "SYSTIME" 0x4041
          , Comment "DATE$: Day of year (year, month, day)"
          , SymEquate "SYSDATE" 0x4044
          , Comment "HIFH$: DOS highest unused RAM address"
          , SymEquate "HIFH" 0x404a
          , Comment "4080 - 41FF: Basic reserved area. L2INIRESRVD initializes this area"
          , SymEquate "BASICRESV" 0x4080
          , SymEquate "USRFNPTR" 0x408e
          , Comment "INKEY$ storage"
          , SymEquate "INKEYSTO" 0x4099
          , Comment "Error code for RESUME"
          , SymEquate "RESUMEERC" 0x409a
          , Comment "Printer carriage position"
          , SymEquate "PRCURPOS" 0x409b
          , Comment "Device type flag: -1 = tape, 0 = video, 1 = line printer"
          , SymEquate "DEVTYPEFLAG" 0x409c
          , Comment "PRINT# scratch space"
          , SymEquate "PRNUMWORK" 0x409d
          , Comment "Pointer to lowest address available for string storage"
          , SymEquate "STRINGLO" 0x40a0
          , Comment "BASIC program line number counter, current line being processed"
          , SymEquate "BASLINENO" 0x40a2
          , Comment "Start of BASIC program pointer, first byte where BASIC programs are stored"
          , SymEquate "BASPRGSTART" 0x40a4
          , Comment "Line cursor position"
          , SymEquate "LINECSRPOS" 0x40a6
          , Comment "Input buffer pointer"
          , SymEquate "INPBUFPTR" 0x40a7
          , Comment "RND seed"
          , SymEquate "RNDSEED" 0x40aa
          , Comment "NTF: Numberic Type Flag"
          , Comment "2: Integer"
          , Comment "3: String"
          , Comment "4: Single precision floating point"
          , Comment "8: Double precision floating point"
          , Comment "(see CPDE2HL)"
          , SymEquate "NTF" 0x40af
          , Comment "Top of memory/highest memory address available for string"
          , Comment "storage. Memory above this address pointer is 'reserved'."
          , SymEquate "MEMTOP" 0x40b1
          , Comment "STRWORKPTR: String work area pointer"
          , SymEquate "STRWORKPTR" 0x40b3
          , Comment "String work area (0x40b5 - 0x40d5"
          , SymEquate "STRWORKAREA" 0x40b5
          , Comment "Pointer to next byte of string storage"
          , SymEquate "NEXTSTRPTR" 0x40d6
          , Comment "Double precision accumulator: LSB, LSB, LSB, LSB, LSB, LSB, MSB, EXP"
          , SymEquate "DACC" 0x411d
          , Comment "Integer accumulator: LSB, MSB"
          , Comment "Single precision accumulator, LSB, LSB, MSB, EXP"
          , SymEquate "IACC" 0x4121
          , Comment "\"Hex\" accumulator: integer, single, double precision aligned at"
          , Comment "the same location."
          , SymEquate "HEXACC" 0x4127
          , Comment "TRSDOS command vector redirections. These are also 'JP' instructions"
          , SymEquate "DOSVECCMD" 0x4173
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Memory mapped I/O addresses"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , SymEquate "LPPORT" 0x37e8
          , SymEquate "DSKCMDSTATUS" 0x37ec
          , SymEquate "DSKTRKSEL" 0x37ed
          , SymEquate "DSKSECSEL" 0x37ee
          , SymEquate "DSKDATA" 0x37ef
          , SymEquate "KBDLINE0" 0x3801
          , SymEquate "KBDLINEFNKEYS" 0x3840
          , SymEquate "KBDLINESHIFTS" 0x3880
          , SymEquate "VIDRAM" 0x3c00
          -- These are the locations where the BASIC CLOAD and SYSTEM "*"'s flicker.
          , SymEquate "VIDLINE0RIGHT1" 0x3c3e
          , SymEquate "VIDLINE0RIGHT2" 0x3c32
          , Comment "USR function pointer"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "TRS-80 Model I Level II ROM disassembly:"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment ""
          , DoDisasm 0x0000 0x004f
          , GrabBytes 0x0050 0x0010
          , nextSeg 0x0060 0x0104
          , GrabAscii 0x0105 (0x0110 - 0x0105)
          , GrabBytes 0x0110 1
          , GrabAscii 0x0111 (0x012b - 0x0111)
          , GrabBytes 0x012b 2
          , nextSeg 0x012d 0x0339
          , Comment "This is an alternate entry point into CHARPRINT, which preserves"
          , Comment "DE. Note that CHARPRINT will load DE with the DO DCB."
          , nextSeg 0x033a 0x03c1
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
          , nextSeg 0x03c2 0x03e2
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
          , nextSeg 0x03e3 0x0457
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "DO (display output) DCB function"
          , Comment BC.empty
          , Comment "C has the character to be output on entry."
          , Comment "(IX + 3), (IX + 4): Current memory location in VIDRAM where the"
          , Comment "character is output (DCB SYSINFO bytes 1 and 2)."
          , Comment "(IX + 5): If non-zero, don't actually output the character, but"
          , Comment "do the rest of the character processing."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0458 0x058c
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "PR (line printer output) DCB function"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x058d 0x0673
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Initialize the restart vector table, located at RST08VEC"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0674 0x06d1
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Restart vector initialization table. Yes, this is code that is"
          , Comment "copied into RAM."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "0x4000 -> RST08 redirect"
          , nextSeg 0x06d2 0x06d4
          , Comment "0x4003 -> RST10 redirect"
          , nextSeg 0x06d5 0x06d7
          , Comment "0x4006 -> RST18 redirect"
          , nextSeg 0x06d8 0x06da
          , Comment "0x4009 -> RST20 redirect"
          , nextSeg 0x06db 0x06dd
          , Comment "0x400c -> RST28 redirect"
          , nextSeg 0x06de 0x06e0
          , Comment "0x400f -> RST30 redirect"
          , nextSeg 0x06e1 0x06e3
          , Comment "0x4012 -> RST38 redirect"
          , nextSeg 0x06e4 0x06e6
          , Comment "0x4015: KI (keyboard) device control block"
          , GrabBytes 0x06e7 1
          , JumpTable 0x06e8 2
          , GrabBytes 0x06ea 3
          , GrabAscii 0x06ed 2
          , Comment "0x401d: DO (display output) device control block"
          , GrabBytes 0x06ef 1
          , JumpTable 0x06f0 2
          , GrabBytes 0x06f2 3
          , GrabAscii 0x06f5 2
          , Comment "0x4025: PR (line printer output) device control block"
          , GrabBytes 0x06f7 1
          , JumpTable 0x06f8 2
          , GrabBytes 0x06fa 3
          , GrabAscii 0x06fd 2
          , Comment "0x402d:"
          , nextSeg 0x06ff 0x0704
          , Comment "0x4033: OUTDCB jumps here if the requested device type bit doesn't match"
          , nextSeg 0x0705 0x0707
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "End of the restart vector initialization table."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x0708 0x09d1
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Copy bytes pointed to by HL to the buffer pointed to by DE"
          , Comment "This just exchanges DE and HL, falls through to CPDE2HL"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x09d2 0x09d2
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Copy the bytes pointed to by DE to the buffer pointed to by HL"
          , Comment "A, B, DE and HL are not preserved. NTF (numeric type flag)"
          , Comment "indicates the number of bytes to copy."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x09d3 0x137b
          , Comment "0.0 floating point constant"
          , GrabBytes 0x137c 4
          , Comment "0.5 floating point constant"
          , GrabBytes 0x1380 4
          , Comment "?? floating point constant"
          , GrabBytes 0x1384 4
          , nextSeg 0x1388 0x158a
          , Comment "These four bytes are a floating point constant?"
          , GrabBytes 0x158b 4
          , nextSeg 0x158f 0x164f
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "BASIC verb table -- first character has high bit set"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , HighBitTable 0x1650 (0x1820 - 0x1650)
          , JumpTable 0x1820 (0x18c9 - 0x1820)
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "BASIC error codes:"
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , GrabAscii 0x18c9 2
          , GrabAscii 0x18cb 2
          , GrabAscii 0x18cd 2
          , GrabAscii 0x18cf 2
          , GrabAscii 0x18d1 2
          , GrabAscii 0x18d3 2
          , GrabAscii 0x18d5 2
          , GrabAscii 0x18d7 2
          , GrabAscii 0x18d9 2
          , GrabAscii 0x18db 2
          , GrabAscii 0x18dd 2
          , GrabAscii 0x18df 2
          , GrabAscii 0x18e1 2
          , GrabAscii 0x18e3 2
          , GrabAscii 0x18e5 2
          , GrabAscii 0x18e7 2
          , GrabAscii 0x18e9 2
          , GrabAscii 0x18eb 2
          , GrabAscii 0x18ed 2
          , GrabAscii 0x18ef 2
          , GrabAscii 0x18f1 2
          , GrabAscii 0x18f3 2
          , GrabAscii 0x18f5 2
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "BASIC reserved data table."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , GrabBytes 0x18f7 39
          , GrabAscii 0x191e 5
          , GrabBytes 0x1923 1
          , GrabAscii 0x1924 4
          , GrabBytes 0x1928 1
          , GrabAscii 0x1929 5
          , GrabBytes 0x192e 2
          , GrabAscii 0x1930 5
          , GrabBytes 0x1935 1
          , nextSeg 0x1936 0x1c95
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , Comment "Compare (HL) against character following the RST 08 instruction."
          , Comment BC.empty
          , Comment "Return address is pointed to by SP, which is transferred into HL"
          , Comment "The return address is incremented so that the RET returns to the"
          , Comment "instruction following the character."
          , Comment BC.empty
          , Comment "A is not preserved. (HL) is the character pointed to by the HL"
          , Comment "register."
          , Comment "=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~="
          , nextSeg 0x1c96 0x25d8
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
    nextSeg sAddr eAddr = DoDisasm sAddr (fromIntegral (eAddr - sAddr) :: Z80disp)
