-- | Test harness for the Misosys EDAS-compatible assembler.
module Main where

import Prelude hiding (pred)
import Test.HUnit
import Data.Maybe
import Data.Word
import Data.Bits
import Data.List
import Control.Lens
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as DVU

import Z80
import Z80.MisosysEDAS

main :: IO ()
main = do { _ <- runTestTT edasTests
          ; return ()
          }

edasTests :: Test
edasTests = test [ equateTests
                 , defbTests
                 , defcTests
                 , defsTests
                 , defwTests
                 ]

defbTests :: Test
defbTests = test [ "defb"              ~: (checkAssembly defbAsm)                                   ~=? True
                 , "defbExpected"      ~: (checkByteVectors and defbAsm defbExpected)               ~=? True
                 , "defbProgCtrs"      ~: (checkProgramCounters and defbAsm defbProgCtrs)           ~=? True
                 , "edasManDB"         ~: (checkAssembly edasManDBAsm)                              ~=? True
                 , "edasManDBExpected" ~: (checkByteVectors and edasManDBAsm edasManDBExpected)     ~=? True
                 , "defbProgCtrs"      ~: (checkProgramCounters and edasManDBAsm edasManDBProgCtrs) ~=? True
                 ]
  where
    defbSource = T.intercalate "\n" [ "                DB       00h"
                                    , "                defb     0, 1"
                                    , "                defb     0,1"
                                    , "                DB       00h, 01h, 'c'"
                                    , "                DEFB     'Memory size?'"
                                    , "                db       'Mem ''sz''?'"
                                    ]
    defbExpected = [ DVU.singleton 0
                   , DVU.fromList [0, 1]
                   , DVU.fromList [0, 1]
                   , DVU.fromList [0, 1, charToWord8 'c']
                   , DVU.fromList (textToWord8 "Memory size?")
                   , DVU.fromList (textToWord8 "Mem \'sz\'?")
                   ]
    (_finalPC, defbProgCtrs) = generateExpectedStmtAddresses defbExpected

    -- Parse and assemble the source:
    defbAsm = edasAssemble $ edasParseSequence "defbSource" defbSource

    -- Test case from the EDAS manual:
    edasManDBSrc = T.intercalate "\n" [ "               DB    'This',' ','is',' ','a',' ','test'"
                                      , "               DB    1,2,'buckle your shoe',3,4,'close the door'"
                                      , "               DB    'This is a tes','t'!80H"
                                      ]
    edasManDBExpected = [ DVU.fromList (textToWord8 "This is a test")
                        , DVU.fromList ( [ 1, 2 ]
                                         ++ (textToWord8 "buckle your shoe")
                                         ++ [ 3, 4 ]
                                         ++ (textToWord8 "close the door")
                                       )
                        , DVU.concat [ DVU.fromList (textToWord8 "This is a tes")
                                     , DVU.singleton (((fromIntegral . C.ord) 't') .|. 0x80)
                                     ]
                        ]
    (_finalPC2, edasManDBProgCtrs) = generateExpectedStmtAddresses edasManDBExpected

    edasManDBAsm = edasAssemble $ edasParseSequence "edasManDB" edasManDBSrc

defcTests :: Test
defcTests = test [ "defc"              ~: (checkAssembly defcAsm)                                   ~=? True
                 , "defcExpected"      ~: (checkByteVectors and defcAsm defcExpected)               ~=? True
                 , "defcProgCtrs"      ~: (checkProgramCounters and defcAsm defcProgCtrs)           ~=? True
                 ]
  where
    defcSource = T.intercalate "\n" [ "                Dc       16, 00h"
                                    , "                dC       17, -1"
                                    , "                DC       37, 'A'"
                                    , "                DC       256, 'a' !80H"
                                    ]

    defcExpected = [ DVU.replicate 16 0
                   , DVU.replicate 17 0xff
                   , DVU.replicate 37 ((fromIntegral . C.ord) 'A')
                   , DVU.replicate 256 (((fromIntegral . C.ord) 'a') .|. 0x80)
                   ] :: [(DVU.Vector Z80word)]

    (_finalPC, defcProgCtrs) = generateExpectedStmtAddresses defcExpected

    -- Parse and assemble the source:
    defcAsm = edasAssemble $ edasParseSequence "defcSource" defcSource

defsTests :: Test
defsTests = test [ "defs"              ~: (checkAssembly defsAsm)                                   ~=? True
                 , "defsExpected"      ~: (checkByteVectors and defsAsm defsExpected)               ~=? True
                 , "defsProgCtrs"      ~: (checkProgramCounters and defsAsm defsProgCtrs)           ~=? True
                 ]
  where
    defsSource = T.intercalate "\n" [ "                Ds       16"
                                    , "                dS       17"
                                    , "                DS       37"
                                    , "                DS       256"
                                    ]

    defsExpected = [ DVU.replicate 16 0
                   , DVU.replicate 17 0
                   , DVU.replicate 37 0
                   , DVU.replicate 256 0
                   ] :: [(DVU.Vector Z80word)]

    (_finalPC, defsProgCtrs) = generateExpectedStmtAddresses defsExpected

    -- Parse and assemble the source:
    defsAsm = edasAssemble $ edasParseSequence "defsSource" defsSource

defwTests :: Test
defwTests = test [ "defw"              ~: (checkAssembly defwAsm)                                   ~=? True
                 , "defwExpected"      ~: (checkByteVectors and defwAsm defwExpected)               ~=? True
                 , "defwProgCtrs"      ~: (checkProgramCounters and defwAsm defwProgCtrs)           ~=? True
                 ]
  where
    defwSource = T.intercalate "\n" [ "                Dw       'R', 'o','y'"
                                    , "                dW       'ab'"
                                    , "                DW       10000,1000,100,10,1"
                                    ]

    defwExpected = [ DVU.fromList [ charToWord8 'R', 0, charToWord8 'o', 0, charToWord8 'y', 0 ]
                   , DVU.fromList [ charToWord8 'a', charToWord8 'b' ]
                   , DVU.fromList [ low 10000, high 10000
                                  , low 1000,  high 1000
                                  , low 100,   high 100
                                  , low 10,    high 10
                                  , low 1,     high 1
                                  ]
                   ] :: [(DVU.Vector Z80word)]

    high, low :: Word16 -> Word8
    high x = fromIntegral (x `shiftR` 8)
    low  x = fromIntegral (x .&. 0xff)

    (_finalPC, defwProgCtrs) = generateExpectedStmtAddresses defwExpected

    -- Parse and assemble the source:
    defwAsm = edasAssemble $ edasParseSequence "defwSource" defwSource

equateTests :: Test
equateTests = test [ "equateAsm"    ~: (checkAssembly equatesAsm)                           ~=? True
                   , "equateSyms-1" ~: (checkSymbolsWith and equatesAsm goodSyms)           ~=? True
                   , "equateSyms-2" ~: (checkSymbolsWith or  equatesAsm badSyms)            ~=? False
                   , "equateVals-1" ~: (checkSymbolValuesWith and equatesAsm expectedVals)  ~=? True
                   , "primExprAsm"  ~: (checkAssembly primExprAsm)                          ~=? True
                   , "primExprVals" ~: (checkSymbolValuesWith and primExprAsm primExpected) ~=? True
                   ]
  where
    symEquates = T.intercalate "\n" [ "CON30   EQU   30"
                                    , "CON16   EQU   +10H"
                                    , "CON3    EQU   3"
                                    , "A2      EQU   CON30+CON16"
                                    , "A3      equ   con30+con16*4"
                                    ]
    expectedVals = [ ("con30", 30)
                   , ("con16", 16)
                   , ("CON3",   3)
                   , ("A2",    46)
                   , ("A3",  46*4)                      -- Note lack of operator precedence
                   ]

    -- Parse and assemble the equates
    equatesAsm = edasAssemble $ edasParseSequence "symEquates" symEquates

    -- Symbols that must be present in the final symbol table
    goodSyms = [ "Con30", "CON30", "con30"
               , "CON16", "con16", "cOn16"
               , "CON3",  "Con3"
               , "A2", "a2"
               , "A3", "a3"
               ]

    -- Symbols that will not be present in the final symbol table
    badSyms      = [ "Cno30", "foobar", "frobnats"
               , "a5", "A5"
               , "con", "noc"
               ]

    -- Primary expression testing
    primExpr     = T.intercalate "\n" [ "C1      EQU   .not. 30"
                                  , "C2      EQU   .high. c1"
                                  , "C3      EQU   .low.c1"
                                  , "C4      EQU   .low. c1"
                                  , "C5      EQU   C2.shl.8!c4.EQ.c1"
                                  , "C6      EQU   c2 .shl. 8!c4.eq.c1"
                                  , "C7      EQU   C2.shl.8 ! c4.EQ. C1"
                                  , "C8      EQU   c2<8!C4 .EQ. C1"
                                  , "C9      equ   c1<-8<8!c4 .eq. c1"
                                  ]
    primExpected = [ ("c1", 0xffe1)
                   , ("c2", 0x00ff)
                   , ("c3", 0x00e1)
                   , ("c4", 0x00e1)
                   , ("c5", 0x0000)
                   , ("c6", 0x0000)
                   , ("c7", 0x0000)
                   , ("c8", 0x0000)
                   , ("c9", 0x0000)
                   ] :: [(T.Text, Word16)]

    -- Assembler output for primary expressions
    primExprAsm = edasAssemble $ edasParseSequence "primExpr" primExpr

-- | Check to see if the code assembled correctly.
checkAssembly :: EDASAsmOutput                   -- ^ The assembler's output
              -> Bool
checkAssembly asm =
  case asm of
  Left problems -> error (T.unpack problems)
  Right _       -> True

-- | Check the symbols in the final symbol table, according to a predicate, e.g., 'and' ensures that all symbols are present.
checkSymbolsWith :: ([Bool] -> Bool)                    -- ^ Predicate function
                 -> EDASAsmOutput                       -- ^ Assembler output
                 -> [T.Text]                            -- ^ Symbol names
                 -> Bool
checkSymbolsWith pred asm syms =
  case asm of
    Left problems             -> error (T.unpack problems)
    Right (finalctx, _result) -> pred $ map (existsAsmSymbol finalctx) syms

-- | Check symbol values with a predicate, e.g., 'and' ensures that all values are correct.
checkSymbolValuesWith :: ([Bool] -> Bool)               -- ^ Predicate function
                      -> EDASAsmOutput                  -- ^ Assembler output
                      -> [(T.Text, Word16)]             -- ^ (symbol name, expected value) list
                      -> Bool
checkSymbolValuesWith pred asm syms =
  case asm of
    Left problems             -> error (T.unpack problems)
    Right (finalctx, _result) -> pred $ [ fromMaybe 0 (findAsmSymbol finalctx x) == y | (x, y) <- syms ]

-- | Check the byte vectors with a predicate
checkByteVectors :: ([Bool] -> Bool)
                 -> EDASAsmOutput
                 -> [(DVU.Vector Word8)]
                 -> Bool
checkByteVectors pred asm expected =
  case asm of
    Left problems            -> error (T.unpack problems)
    Right (_finalctx, stmts) -> pred $ zipWith (==) (map (\x -> x ^. bytes & DVU.toList) stmts) (map DVU.toList expected)

-- | Check the expected program counter with a predicate
checkProgramCounters :: ([Bool] -> Bool)
                     -> EDASAsmOutput
                     -> [Z80addr]
                     -> Bool
checkProgramCounters pred asm expected =
  case asm of
    Left problems            -> error (T.unpack problems)
    Right (_finalctx, stmts) -> pred $ zipWith (==) (map (\x -> x ^. stmtAddr) stmts) expected

-- | Convert a character to 'Word8'
charToWord8 :: Char
            -> Word8
charToWord8 = fromIntegral . C.ord

-- Convert 'String' to a list of 'Word8's
stringToWord8 :: String
              -> [Word8]
stringToWord8 = map charToWord8

-- Convert 'Text' to a list of 'Word8's
textToWord8 :: T.Text
            -> [Word8]
textToWord8 = (map charToWord8) . T.unpack

-- | Generate statement addresses from the expected byte vector output
generateExpectedStmtAddresses :: (DVU.Unbox wordType) => [DVU.Vector wordType]
                              -> (Z80addr, [Z80addr])
generateExpectedStmtAddresses = mapAccumL (\pc elt -> (pc + (fromIntegral . DVU.length) elt, pc)) (0 :: Z80addr)
