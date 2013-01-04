-- | Test harness for the Misosys EDAS-compatible assembler.
module Main where

import Prelude hiding (pred)
import Test.HUnit
import Data.Maybe
import Data.Word
import qualified Data.Text as T

import Z80.MisosysEDAS

main :: IO ()
main = do { _ <- runTestTT edasTests
          ; return ()
          }

edasTests :: Test
edasTests = test [ "equateAsm"    ~: (checkAssembly equatesAsm)                           ~=? True
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
    badSyms  = [ "Cno30", "foobar", "frobnats"
               , "a5", "A5"
               , "con", "noc"
               ]

    -- Primary expression testing
    primExpr = T.intercalate "\n" [ "C1      EQU   .not. 30"
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

-- Check to see if the code assembled correctly
checkAssembly :: EDASAsmOutput                   -- ^ The assembler's output
              -> Bool
checkAssembly asm =
  case asm of
  Left problems -> error (T.unpack problems)
  Right _       -> True

-- Check the symbols in the final symbol table, according to a predicate, e.g., 'and' ensures that all symbols are present.
checkSymbolsWith :: ([Bool] -> Bool)                    -- ^ Predicate function
                 -> EDASAsmOutput                       -- ^ Assembler output
                 -> [T.Text]                            -- ^ Symbol names
                 -> Bool
checkSymbolsWith pred asm syms =
  case asm of
    Left problems             -> error (T.unpack problems)
    Right (finalctx, _result) -> pred $ map (existsAsmSymbol finalctx) syms

-- Check symbol values with a predicate, e.g., 'and' ensures that all values are correct.
checkSymbolValuesWith :: ([Bool] -> Bool)               -- ^ Predicate function
                      -> EDASAsmOutput                  -- ^ Assembler output
                      -> [(T.Text, Word16)]             -- ^ (symbol name, expected value) list
                      -> Bool
checkSymbolValuesWith pred asm syms =
  case asm of
    Left problems             -> error (T.unpack problems)
    Right (finalctx, _result) -> pred $ [ fromMaybe 0 (findAsmSymbol finalctx x) == y | (x, y) <- syms ]
