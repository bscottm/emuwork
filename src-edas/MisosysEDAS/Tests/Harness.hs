-- | Test harness for the Misosys EDAS-compatible assembler.
module Main where

import Prelude hiding (pred)

-- import Debug.Trace

import Test.HUnit hiding (showPath)
-- import Data.Monoid
import Data.Maybe
import Data.Word
import Data.Bits
import Control.Lens
import Control.Monad
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as DVU

import Machine
import Z80
import Z80.MisosysEDAS

-- Show each of the tests as they execute, instead of the summary that 'runTestTT' would normally output.
main :: IO ()
main = do { (counts', _) <- performTest reportStart reportError reportFailure () edasTests
          ; putStrLn (showCounts counts')
          ; return ()
          }
  where
    reportStart ss _us = TIO.putStrLn $ (showPath (path ss))
    reportError   = reportProblem "Error:"   "Error in:   "
    reportFailure = reportProblem "Failure:" "Failure in: "
    reportProblem p0 p1 _loc msg ss _us = TIO.putStrLn line
     where line  = T.concat [ "### "
                            , kind
                            , path'
                            , "\n"
                            , T.pack msg
                            ]
           kind  = T.pack (if T.null path' then p0 else p1)
           path' = showPath (path ss)
    showPath [] = T.empty
    showPath nodes = foldl1 f (map showNode nodes)
     where f b a = T.concat [ a, ":",  b ]
           showNode (ListItem n) = T.justifyRight 3 ' ' ((T.pack . show) n)
           showNode (Label label) = T.pack $ safe label (show label)
           safe s ss = if ':' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s


infix 1 @!?

-- | Boolean false assertion: Expect that the result of the test will be 'False'.
(@!?) :: (AssertionPredicable t) =>
         t
      -> String
      -> Assertion
pred @!? msg = assertionPredicate pred >>= (\b -> when b (assertFailure msg))

-- | The list of tests that 'HUnit' will execute.
edasTests :: Test
edasTests = test [ defbTests
                 , defcTests
                 , defsTests
                 , defwTests
                 , dateTimeTest
                 , dsymTests
                 , deflTests
                 , endEntryTests
                 , equateTests
                 , dupLabelTests
                 , originTests
                 , condAsmEvalTests
                 ]

-- | "Assembler pass failed" error message
asmPassFailed :: String
asmPassFailed = "Assembler pass failed"
-- | "Byte vector mismatch" error message
byteVecMismatch :: String
byteVecMismatch = "Byte vector mismatch" :: String
-- | "Expected lengths/program counters don't match" error message
progCtrsMismatch :: String
progCtrsMismatch = "Program counters or expected byte vector length don't match" :: String
-- | The EDAS assembler pass unepxectedly succeeded
unexpectedAsmPassSucceeded :: String
unexpectedAsmPassSucceeded = "EDAS assembler pass unexpectedly succeeded -- failure was expected."

-- | "DB"/"DEFB" (define bytes) tests. Ensures that code will assemble correctly, generated byte vectors are correct,
-- and with the proper lengths.
defbTests :: Test
defbTests = test [ "defb"              ~: (checkAssembly defbAsm False)                             @? asmPassFailed
                 , "defbExpected"      ~: (checkByteVectors and defbAsm defbExpected)               @? byteVecMismatch
                 , "edasManDB"         ~: (checkAssembly edasManDBAsm False)                        @? asmPassFailed
                 , "edasManDBExpected" ~: (checkByteVectors and edasManDBAsm edasManDBExpected)     @? byteVecMismatch
                 ]
  where
    defbSource = T.intercalate "\n" [ "                DB       00h"
                                    , "                defb     0, 1"
                                    , "                defb     0,1"
                                    , "                DB       00h, 01h, 'c'"
                                    , "                DEFB     'Memory size?'"
                                    , "                db       'Mem ''sz''?'"
                                    , "                end      0000H"
                                    ]
    defbExpected = [ DVU.singleton 0
                   , DVU.fromList [0, 1]
                   , DVU.fromList [0, 1]
                   , DVU.fromList [0, 1, charToWord8 'c']
                   , DVU.fromList (textToWord8 "Memory size?")
                   , DVU.fromList (textToWord8 "Mem \'sz\'?")
                   , DVU.empty
                   ]

    -- Parse and assemble the source:
    defbAsm = edasAssemble $ edasParseSequence "defbSource" defbSource

    -- Test case from the EDAS manual:
    edasManDBSrc = T.intercalate "\n" [ "               DB    'This',' ','is',' ','a',' ','test'"
                                      , "               DB    1,2,'buckle your shoe',3,4,'close the door'"
                                      , "               DB    'This is a tes','t'!80H"
                                      , "               end   0000H"
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
                        , DVU.empty
                        ]

    edasManDBAsm = edasAssemble $ edasParseSequence "edasManDB" edasManDBSrc

-- | "DC" (define constant space/fill) tests. See 'defbTests' for what is checked.
defcTests :: Test
defcTests = test [ "defc"              ~: (checkAssembly defcAsm False)                   @? asmPassFailed
                 , "defcExpected"      ~: (checkByteVectors and defcAsm defcExpected)     @? byteVecMismatch
                 ]
  where
    defcSource = T.intercalate "\n" [ "                Dc       16, 00h"
                                    , "                dC       17, -1      ; Prime number of constants"
                                    , "                DC       37, 'A'"
                                    , "                DC       256, 'a' !80H"
                                    , "                end      0000H"
                                    ]

    defcExpected = [ DVU.replicate 16 0
                   , DVU.replicate 17 0xff
                   , DVU.replicate 37 ((fromIntegral . C.ord) 'A')
                   , DVU.replicate 256 (((fromIntegral . C.ord) 'a') .|. 0x80)
                   , DVU.empty
                   ] :: [(DVU.Vector Z80byte)]

    -- Parse and assemble the source:
    defcAsm = edasAssemble $ edasParseSequence "defcSource" defcSource

-- | "DS" (define space) tests -- "DS" is essentially the same as "DC" with a fill constant of 0.
defsTests :: Test
defsTests = test [ "defs"              ~: (checkAssembly defsAsm False)                   @? asmPassFailed
                 , "defsExpected"      ~: (checkByteVectors and defsAsm defsExpected)     @? byteVecMismatch
                 ]
  where
    defsSource = T.intercalate "\n" [ "                Ds       16"
                                    , "                dS       17"
                                    , "                DS       37"
                                    , "                DS       256"
                                    , "                EnD      0000H"
                                    ]

    defsExpected = [ DVU.replicate 16 0
                   , DVU.replicate 17 0
                   , DVU.replicate 37 0
                   , DVU.replicate 256 0
                   , DVU.empty
                   ] :: [(DVU.Vector Z80byte)]

    -- Parse and assemble the source:
    defsAsm = edasAssemble $ edasParseSequence "defsSource" defsSource

-- | "DW"/"DEFW" (define words). Note that the Z80 is a little endian architecture.
defwTests :: Test
defwTests = test [ "defw"              ~: (checkAssembly defwAsm False)                   @? asmPassFailed
                 , "defwExpected"      ~: (checkByteVectors and defwAsm defwExpected)     @? byteVecMismatch
                 ]
  where
    defwSource = T.intercalate "\n" [ "                Dw       'R', 'o','y'"
                                    , "                dW       'ab'"
                                    , "                DW       10000,1000,100,10,1"
                                    , "                END      0FAA0H"
                                    ]

    defwExpected = [ DVU.fromList [ charToWord8 'R', 0, charToWord8 'o', 0, charToWord8 'y', 0 ]
                   , DVU.fromList [ charToWord8 'a', charToWord8 'b' ]
                   , DVU.fromList [ low 10000, high 10000
                                  , low 1000,  high 1000
                                  , low 100,   high 100
                                  , low 10,    high 10
                                  , low 1,     high 1
                                  ]
                   , DVU.empty
                   ] :: [(DVU.Vector Z80byte)]

    high, low :: Word16 -> Word8
    high x = fromIntegral (x `shiftR` 8)
    low  x = fromIntegral (x .&. 0xff)

    -- Parse and assemble the source:
    defwAsm = edasAssemble $ edasParseSequence "defwSource" defwSource

-- | "DATE" and "TIME" pseudo-operation tests. This cannot check the exact date/time that's generated; however,
-- this test just ensures that something sensible is generated.
dateTimeTest :: Test
dateTimeTest = test [ "datetimeAsm"       ~: (checkAssembly dateTimeAsm False)                   @? asmPassFailed
                    , "datetimeProgCtrs"  ~: (checkByteVectorLengths and dateTimeAsm theLengths) @? progCtrsMismatch
                    ]
  where
    theSource   = T.intercalate "\n" [ "                DATE"
                                     , "                TIME"
                                     , "                END     0AA00H"
                                     ]

    theLengths  = [ T.length "MM/DD/YY"
                  , T.length "HH:MM:SS"
                  , 0
                  ]

    dateTimeAsm = edasAssemble $ edasParseSequence "dateTimeTestSource" theSource

-- | "DSYM" and "DX" tests.
dsymTests :: Test
dsymTests = test [ "dsym"              ~: (checkAssembly dsymAsm False)                   @? asmPassFailed
                 , "dsymExpected"      ~: (checkByteVectors and dsymAsm dsymExpected)     @? byteVecMismatch
                 ]
  where
    dsymSource = T.intercalate "\n" [ "         ORG     0E143H"
                                    , "LABEL:"
                                    , "         DSYM    LABEL"
                                    , "         DX      LABEL"
                                    , "         END     0F130H"
                                    ]

    dsymExpected = [ DVU.empty
                   , DVU.empty
                   , DVU.fromList $ textToWord8 "LABEL"
                   , DVU.fromList [ 0x43, 0xe1 ]
                   , DVU.empty
                   ] :: [(DVU.Vector Z80byte)]

    -- Parse and assemble the source:
    dsymAsm = edasAssemble $ edasParseSequence "dsymSource" dsymSource

-- | "DEFL" tests
deflTests :: Test
deflTests = test [ "defl"              ~: (checkAssembly deflAsm False)                   @? asmPassFailed
                 , "deflExpected"      ~: (checkByteVectors and deflAsm deflExpected)     @? byteVecMismatch
                 ]
  where
    deflSource = T.intercalate "\n" [ "         ORG     8080H"
                                    , "prog$    defl    $         ; Program code"
                                    , "         org     8100H"
                                    , "         db      'This is a message'"
                                    , "data$    defl    $"
                                    , ""
                                    , "         org     prog$     ; Back to program code"
                                    , "         nop"
                                    , "prog$    defl    $"
                                    , ""
                                    , "         org     data$     ; More data"
                                    , "         db      'More message data'"
                                    , ""
                                    , "data$    defl    $"
                                    , "         org     prog$"
                                    , "         end     prog$"
                                    ]

    deflExpected =  [ DVU.empty
                    , DVU.empty
                    , DVU.empty
                    , DVU.fromList $ textToWord8 "This is a message"
                    , DVU.empty
                    , DVU.empty
                    , DVU.empty
                    , DVU.empty -- FIXME: Should be DVU.singleton 0 when instructions are generated.
                    , DVU.empty
                    , DVU.empty
                    , DVU.empty
                    , DVU.fromList $ textToWord8 "More message data"
                    , DVU.empty
                    , DVU.empty
                    , DVU.empty
                    , DVU.empty
                    ]:: [(DVU.Vector Z80byte)]

    -- Parse and assemble the source:
    deflAsm = edasAssemble $ edasParseSequence "deflSource" deflSource

-- | "END" and "ENTRY" tests
endEntryTests :: Test
endEntryTests = test [ "end1"            ~: (checkAssembly endAsm1 False)                   @? asmPassFailed
                     , "end1saddr"       ~: (checkStartAddress 0x8080 endAsm1)              @? "Start address doesn't match"
                     , "end2"            ~: (checkAssembly endAsm2 False)                   @? asmPassFailed
                     , "end2saddr"       ~: (checkStartAddress 0x8081 endAsm2)              @? "Start address doesn't match"
                     ]
  where
    endAsm1Source = T.intercalate "\n" [ "         ORG     8080H"
                                       , "START:"
                                       , "         end     start"
                                       ]

    endAsm2Source = T.intercalate "\n" [ "         ORG     8080H"
                                       , "START:"
                                       , "         ENTRY   START + 1"
                                       , "         end"
                                       ]

    -- Parse and assemble the source:
    endAsm1 = edasAssemble $ edasParseSequence "endAsm1Source" endAsm1Source
    endAsm2 = edasAssemble $ edasParseSequence "endAsm2Source" endAsm2Source

-- | Symbol equate and expression tests. This checks for a successful assembler pass, that the symbols inserted are
-- present, that bad/extraneous/invalid symbols are not in the final symbol table, and checks that expression parsing
-- and evaluation functions correctly. 
--
-- One thing to note about EDAS: it never had a concept of parenthesized expressions or mathematical precedence; evaluation
-- is strictly from left to right.
equateTests :: Test
equateTests = test [ "equateAsm"    ~: (checkAssembly equatesAsm False)                     @? asmPassFailed
                   , "equateSyms-1" ~: (checkSymbolsWith and equatesAsm goodSyms)           @? "Symbols missing from symtab"
                   , "equateSyms-2" ~: (liftM not $ checkSymbolsWith or equatesAsm badSyms) @? "Extraneous symbols in symtab"
                   , "equateVals-1" ~: (checkSymbolValuesWith and equatesAsm expectedVals)  @? "Expected evaluation failed"
                   , "primExprAsm"  ~: (checkAssembly primExprAsm False)                    @? asmPassFailed
                   , "primExprVals" ~: (checkSymbolValuesWith and primExprAsm primExpected) @? "Expected evaluation failed"
                   ]
  where
    symEquates = T.intercalate "\n" [ "        ORG   4001H          ; Sets a4 to some value other than 0"
                                    , "CON30   EQU   30             ; An example from the EDAS manual"
                                    , "CON16   EQU   +10H"
                                    , "CON3    EQU   3"
                                    , "A2      EQU   CON30+CON16"
                                    , "A3      equ   con30+con16*4"
                                    , "a4      equ   $              ; should be 4001H"
                                    , "$abc    equ   0E010H"
                                    , "a5      equ   $abc + 2"
                                    , "a6      equ   1001001b"
                                    , "a7      equ   137q"
                                    , "a8      equ   124o"
                                    , "        end   0000H"
                                    ]
    expectedVals = [ ("con30",    30)
                   , ("CoN30",    30)
                   , ("cOn30",    30)
                   , ("con16",    16)
                   , ("CON3",      3)
                   , ("A2",       46)
                   , ("A3",     46*4)                   -- Note lack of operator precedence
                   , ("A4",   0x4001)
                   , ("$abc", 0xe010)
                   , ("a5",   0xe012)
                   , ("a6",   0x0049)
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
                   , "a51", "A5x"
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
                                      , "        end   1234H"
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

-- | Duplicate label test. This is an expected failure.
dupLabelTests :: Test
dupLabelTests = test [ "dupLabelAsm"  ~: (checkAssembly dupLabelAsm True)                   @? unexpectedAsmPassSucceeded
                     , "dupLabelAsm2" ~: (checkAssembly dupLabelAsm2 True)                  @? unexpectedAsmPassSucceeded
                     ]
  where
    dupLabelSource = T.intercalate "\n" [ "        ORG   4001H"
                                        , "label:"
                                        , "label:"
                                        , "        END   label"
                                        ]

    dupLabelSource2 = T.intercalate "\n" [ "        ORG   4001H"
                                         , "c0      equ   01FF0H"
                                         , "c0      equ   0"
                                         , "        end   c0"
                                         ]

    -- Parse and assemble the duplicate label tests
    dupLabelAsm  = edasAssemble $ edasParseSequence "dupLabel"  dupLabelSource
    dupLabelAsm2 = edasAssemble $ edasParseSequence "dupLabel2" dupLabelSource2

-- | Assembly origin tests, specifically check that a label on an origin pseudo-operation is rejected.
originTests :: Test
originTests = test [ "originAsm"  ~: (checkAssembly originSymbolAsm True)                   @? unexpectedAsmPassSucceeded
                   ]
  where
    originSymbolAsmSource = T.intercalate "\n" [ "label   ORG   4001H"
                                               , "        END   label"
                                               ]

    -- Parse and assemble the duplicate label tests
    originSymbolAsm  = edasAssemble $ edasParseSequence "originSymbolAsm"  originSymbolAsmSource

-- | Conditional assembly: evaluate the expression for true/false (0,!0)
condAsmEvalTests :: Test
condAsmEvalTests = test [ "condEvalAsm1"   ~: (checkAssembly condEvalAsm1 False)            @? asmPassFailed
                        , "condEvalAsm2"   ~: (checkAssembly condEvalAsm2 True)             @? unexpectedAsmPassSucceeded
                        , "condEvalAsm3"   ~: (checkAssembly condEvalAsm3 False)            @? asmPassFailed
                        , "condEvalAsm4"   ~: (checkAssembly condEvalAsm4 False)            @? asmPassFailed
                        , "condEvalAsm5"   ~: (checkAssembly condEvalAsm5 False)            @? asmPassFailed
                        , "condEvalAsm6"   ~: (checkAssembly condEvalAsm6 False)            @? asmPassFailed
                        , "condEvalAsm7"   ~: (checkAssembly condEvalAsm7 True)             @? unexpectedAsmPassSucceeded
                        ]
  where
    condEvalAsmSource1 = T.intercalate "\n" [ "        ORG   4001H"
                                            , "        IF    0"
                                            , "        ; This is within the 'IF 0' statements"
                                            , "        ; This should never get assembled"
                                            , ""
                                            , "        db     'bad'"
                                            , "        ELSE"
                                            , "        ; Code following 'ELSE'"
                                            , "        db     'good'"
                                            , "        ENDIF"
                                            , "        END   4001H"
                                            ]

    condEvalAsmSource2 = T.intercalate "\n" [ "        IF    0"
                                            , "        ELSE"
                                            ]
 
    condEvalAsmSource3 = T.intercalate "\n" [ "        IF    0"
                                            , "        ELSE"
                                            , "        ENDIF"
                                            , "        END   0000H"
                                            ]
 
    condEvalAsmSource4 = T.intercalate "\n" [ "        IF    0"
                                            , "        ENDIF"
                                            , "        END   0000H"
                                            ]

    condEvalAsmSource5 = T.intercalate "\n" [ "        IF    0"
                                            , "          IF    0"
                                            , "          ELSE"
                                            , "          ENDIF"
                                            , "        ENDIF"
                                            , "        END   0000H"
                                            ]

    condEvalAsmSource6 = T.intercalate "\n" [ "        IF    0"
                                            , "          IF    0"
                                            , "          ENDIF"
                                            , "          IF    0"
                                            , "            IF    0"
                                            , "            ENDIF"
                                            , "          ELSE"
                                            , "            IF    0"
                                            , "            ELSE"
                                            , "            ENDIF"
                                            , "          ENDIF"
                                            , "        ENDIF"
                                            , "        END   0000H"
                                            ]

    condEvalAsmSource7 = T.intercalate "\n" [ "        IF    0"
                                            , "          IF    0            ; this is missing an 'ENDIF'"
                                            , "          IF    0"
                                            , "            IF    0"
                                            , "            ENDIF"
                                            , "          ELSE"
                                            , "            IF    0"
                                            , "            ELSE"
                                            , "            ENDIF"
                                            , "          ENDIF"
                                            , "        ENDIF"
                                            , "        END   0000H"
                                            ]

    -- Parse and assemble the conditional assembly tests
    condEvalAsm1  = edasAssemble $ edasParseSequence "condEvalAsm1"  condEvalAsmSource1
    condEvalAsm2  = edasAssemble $ edasParseSequence "condEvalAsm2"  condEvalAsmSource2
    condEvalAsm3  = edasAssemble $ edasParseSequence "condEvalAsm3"  condEvalAsmSource3
    condEvalAsm4  = edasAssemble $ edasParseSequence "condEvalAsm4"  condEvalAsmSource4
    condEvalAsm5  = edasAssemble $ edasParseSequence "condEvalAsm5"  condEvalAsmSource5
    condEvalAsm6  = edasAssemble $ edasParseSequence "condEvalAsm6"  condEvalAsmSource6
    condEvalAsm7  = edasAssemble $ edasParseSequence "condEvalAsm7"  condEvalAsmSource7

-- | Check to see if the code assembled correctly.
checkAssembly :: EDASAsmOutput                          -- ^ The assembler's output
              -> Bool                                   -- ^ Expected failure flag: True -> failure expected
              -> IO Bool
checkAssembly asm expectFail = checkAssemblerWarnings asm True >> liftM checkAsm asm
  where
    checkAsm theAsm = case theAsm of
                        Left problems -> if not expectFail then
                                           error (T.unpack problems)
                                         else
                                           True       -- Failure was expected
                        Right _       -> not expectFail

-- | Check the symbols in the final symbol table, according to a predicate, e.g., 'and' ensures that all symbols are present.
checkSymbolsWith :: ([Bool] -> Bool)                    -- ^ Predicate function
                 -> EDASAsmOutput                       -- ^ Assembler output
                 -> [T.Text]                            -- ^ Symbol names
                 -> IO Bool
checkSymbolsWith pred asm syms = liftM checkSyms asm
  where
    checkSyms theAsm = case theAsm of
                         Left problems             -> error (T.unpack problems)
                         Right (finalctx, _result) -> pred $ map (\sym -> existsAsmSymbol sym finalctx) syms

-- | Check symbol values with a predicate, e.g., 'and' ensures that all values are correct.
checkSymbolValuesWith :: ([Bool] -> Bool)               -- ^ Predicate function
                      -> EDASAsmOutput                  -- ^ Assembler output
                      -> [(T.Text, Word16)]             -- ^ (symbol name, expected value) list
                      -> IO Bool
checkSymbolValuesWith pred asm syms = liftM checkSVals asm
  where
    checkSVals theAsm = case theAsm of
                          Left problems             -> error (T.unpack problems)
                          Right (finalctx, _result) -> pred $ [ fromMaybe 0 (findAsmSymbol x finalctx) == y | (x, y) <- syms ]

-- | Check the byte vectors with a predicate
checkByteVectors :: ([Bool] -> Bool)                    -- ^ Predicate function that compares expected to actual values
                 -> EDASAsmOutput                       -- ^ The assembler's output
                 -> [(DVU.Vector Word8)]                -- ^ Expected generated bytes
                 -> IO Bool                             -- ^ Test result
checkByteVectors pred asm expected = liftM checkBVecs asm
  where
    checkBVecs theAsm = case theAsm of
                          Left problems            -> error (T.unpack problems)
                          Right (_finalctx, stmts) -> pred $ zipWith (==)
                                                                     (map (\x -> x ^. bytes & DVU.toList) stmts)
                                                                     (map DVU.toList expected)

-- | Check the byte vector lengths, combined with a result predicate
checkByteVectorLengths :: ([Bool] -> Bool)              -- ^ Predicate function that compares expected to actual values
                       -> EDASAsmOutput                 -- ^ The assembler's output
                       -> [Int]                         -- ^ Expected generated bytes
                       -> IO Bool                       -- ^ Test result
checkByteVectorLengths pred asm expected = liftM checkBVecLengths asm
  where
    checkBVecLengths theAsm = case theAsm of
                                Left problems            -> error (T.unpack problems)
                                Right (_finalctx, stmts) -> pred $ zipWith (==)
                                                                           (map (\x -> x ^. bytes & DVU.length) stmts)
                                                                           expected

-- | Check the expected program counter with a predicate
checkProgramCounters :: ([Bool] -> Bool)              -- ^ Predicate function that compares the expected to actual PCs.
                     -> EDASAsmOutput                 -- ^ The assembler's output
                     -> [Z80addr]                     -- ^ Expected program counter values
                     -> IO Bool                       -- ^ Test result
checkProgramCounters pred asm expected = liftM checkPCs asm
  where
    checkPCs theAsm = case theAsm of
                        Left problems            -> error (T.unpack problems)
                        Right (_finalctx, stmts) -> pred $ zipWith (==) (stmtAddrs stmts) expected
    stmtAddrs stmts = map (\x -> x ^. stmtAddr) stmts

-- | Check the start address of the assembler\'s output
checkStartAddress :: Z80addr
                  -> EDASAsmOutput
                  -> IO Bool
checkStartAddress sAddr asm = liftM checkSAddr asm
  where
    checkSAddr theAsm = case theAsm of
                          Left problems       -> error (T.unpack problems)
                          Right (ctx, _stmts) ->
                            let asmStartAddr = ctx ^. startAddr
                                sAddrsMatch  = (isJust asmStartAddr) && (fromJust asmStartAddr) == sAddr
                            in  if sAddrsMatch then
                                  sAddrsMatch
                                else
                                  error (T.unpack $ T.concat [ "sAddr = "
                                                             , as0xHex sAddr
                                                             , ", got "
                                                             , (T.pack . show . (liftM as0xHex)) asmStartAddr
                                                             ]
                                        )

-- | Check for assembler warnings, dump them out if requested
checkAssemblerWarnings :: EDASAsmOutput
                       -> Bool
                       -> IO Bool
checkAssemblerWarnings asm showWarnings = when showWarnings dumpWarnings
                                          >> liftM checkAsmWarnings asm
  where
    -- There has to be a nicer way to implement 'dumpWarnings'
    dumpWarnings            = asm
                              >>= (\theAsm ->
                                    case theAsm of
                                      Left _problems      -> return ()
                                      Right (ctx, _stmts) -> mapM_ TIO.putStrLn (ctx ^. warnings)
                                 )
    checkAsmWarnings theAsm = case theAsm of
                                Left _problems      -> False
                                Right (ctx, _stmts) -> null (ctx ^. warnings)

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
textToWord8 = stringToWord8 . T.unpack
