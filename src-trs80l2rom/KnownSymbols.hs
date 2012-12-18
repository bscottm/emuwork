module KnownSymbols where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import Z80

knownSymbols :: Map Z80addr T.Text
knownSymbols   = Map.fromList [ (0x0000, "RST00")
                              , (0x0008, "RST08")
                              , (0x0010, "RST10")
                              , (0x0013, "WANTDCBGET")
                              , (0x0018, "RST18")
                              , (0x001b, "WANTDCBPUT")
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
                              , (0x033a, "COUT")
                              , (0x03c2, "OUTDCB")
                              , (0x03dd, "OUTDCBFINISH")
                              , (0x03e3, "KBDREAD")
                              , (0x03eb, "NXTKEYLINE")
                              , (0x0458, "DODCBFN")
                              , (0x0506, "CURCON")
                              , (0x058d, "PRDCBFN")
                              , (0x05d1, "LPREADY")
                              , (0x05d9, "DOKBLINE")
                              , (0x0674, "INITRSTVECS")
                              , (0x06d2, "RSTVECTABLE")
                              , (0x09d2, "CPHL2DE")
                              , (0x09d3, "CPDE2HL")
                              -- Integer math functions
                              , (0x0bd2, "INTADD")
                              , (0x0bc7, "INTSUB")
                              , (0x0bf2, "INTMPY")
                              -- Floating point constants
                              , (0x137c, "SFPZERO")
                              , (0x1380, "SFPHALF")
                              -- BASIC trancendental functions
                              , (0x1541, "BASCOS")
                              , (0x1547, "BASSIN")
                              , (0x15a8, "BASTAN")
                              , (0x15bd, "BASATN")
                              -- BASIC's high-bit verb table
                              , (0x1650, "VERBS")
                              , (0x1822, "VERBDISPATCH")
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
                              , (0x18f7, "BASRSVINIT")
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
                              , (0x2490, "INTDIV")
                              , (0x25d9, "NUMTYPE")
                              , (0x2608, "BASDIM")
                              ]
