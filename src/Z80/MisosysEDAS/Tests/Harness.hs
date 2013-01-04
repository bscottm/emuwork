module Main where

import Prelude hiding (pred)
import Test.HUnit
import Data.Maybe
import qualified Data.Text as T

import Z80.MisosysEDAS

main :: IO ()
main = do { _ <- runTestTT edasTests
          ; return ()
          }

edasTests :: Test
edasTests = test [ "equateAsm"    ~: checkEquatesAsm                      ~=? True
                 , "equateSyms-1" ~: (checkSymbols and goodSyms)          ~=? True
                 , "equateSyms-2" ~: (checkSymbols or  badSyms)           ~=? False
                 , "equateVals-1" ~: (checkSymbolValues and expectedVals) ~=? True
                 ]
  where
    symEquates = T.intercalate "\n" [ "CON30   EQU   30"
                                    , "CON16   EQU   +10H"
                                    , "CON3    EQU   3"
                                    , "A2      EQU   CON30+CON16"
                                    , "A3      equ   con30+con16*4"
                                    ]
    equatesAsm = edasAssemble $ edasParseSequence "symEquates" symEquates
    -- Check to see if the code assembled correctly
    checkEquatesAsm =
      case equatesAsm of
        Left problems -> error (T.unpack problems)
        Right _       -> True
    -- Check the symbols in the final symbol table
    checkSymbols pred syms =
      case equatesAsm of
        Left problems             -> error (T.unpack problems)
        Right (finalctx, _result) -> pred $ map (existsAsmSymbol finalctx) syms
    goodSyms = [ "Con30", "CON30", "con30"
               , "CON16", "con16", "cOn16"
               , "CON3",  "Con3"
               , "A2", "a2"
               , "A3", "a3"
               ]
    badSyms  = [ "Cno30", "foobar", "frobnats"
               , "a5", "A5"
               , "con", "noc"
               ]
    checkSymbolValues pred syms =
      case equatesAsm of
        Left problems             -> error (T.unpack problems)
        Right (finalctx, _result) -> pred $ [ fromMaybe 0 (findAsmSymbol finalctx x) == y | (x, y) <- syms ]
    -- Check symbol values
    expectedVals = [ ("con30", 30)
                   , ("con16", 16)
                   , ("CON3",   3)
                   , ("A2",    46)
                   , ("A3",  46*4)
                   ]
