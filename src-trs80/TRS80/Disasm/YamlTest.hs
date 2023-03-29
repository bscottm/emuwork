{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad         (foldM, unless, when, guard)

import qualified Data.ByteString       as S
import           Data.ByteString.Lazy  (ByteString, toChunks)
import           Data.List             (intercalate, foldl')
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E (decodeUtf8)
import qualified Data.Yaml             as Y

import           System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (RequireOrder), OptDescr (..), getOpt)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)
import           System.IO             (hPutStrLn, stderr)
import           Test.HUnit            (Assertion, AssertionPredicable (..), Counts (errors, failures), Node (Label, ListItem),
                                        State (path), Test, Testable (test), assertFailure, performTest, showCounts, (@?), (~:))
import           Text.RawString.QQ     (r)

import TRS80.Disasm.Guidance ( yamlStringGuidance )

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
  (getArgs >>= combineArgs . getOpt RequireOrder testOptions)
    >>= mkYAMLTests
    >>= runYAMLTests
  where
    -- We get a triple, not three separate arguments:
    combineArgs (optsActions, rest, errs) =
          unless (null errs) (hPutStrLn stderr (intercalate "\n" errs) >> showUsage >> exitFailure)
          >> unless (null rest) showUsage
          >> foldM (flip ($)) mkTestArgs optsActions

    runYAMLTests tests = do
      (counts', _) <- performTest reportStart reportError reportFailure () tests
      putStrLn (intercalate "\n" ["", "Summary:", showCounts counts'])
      when (errors counts' == 0 || failures counts' == 0) exitSuccess
      exitFailure
  
    reportStart ss _us = putStrLn . T.unpack . showPath . path $ ss
    reportError   = reportProblem "Error"
    reportFailure = reportProblem "Failure"
    reportProblem msgType _loc msg ss _us = putStrLn line
      where
        line  = "### " ++ msgType ++ if null path' then ":" else " in:" ++ path' ++ "\n" ++ msg
        path' = T.unpack. showPath . path $ ss

    showPath [] = T.empty
    showPath nodes = T.concat ["++ ", T.intercalate ":" (map showNode labelPath)]
     where
      labelPath              = reverse (filter onlyLabels nodes)
      onlyLabels (Label _)   = True
      onlyLabels _           = False
      showNode (ListItem _n) = T.empty
      showNode (Label label) = T.justifyLeft 9 ' ' (T.pack . safe $ label)
      safe s
        | ':' `elem` s || head s == '"'
        = show s
        | otherwise
        = s

data TestArgs =
  TestArgs
  { showContent :: Bool
  , showResult  :: Bool
  , showFail    :: Bool
  }

mkTestArgs :: TestArgs
mkTestArgs = TestArgs
                { showContent = False
                , showResult  = False
                , showFail    = False
                }

testOptions :: [OptDescr (TestArgs -> IO TestArgs)]
testOptions =
 [ Option [] ["show-content"] (NoArg setShowContent) "Show test string's contents"
 , Option [] ["show-result"]  (NoArg setShowResult)  "Show test's result"
 , Option [] ["show-fail"]    (NoArg setShowFail)    "Show failure output"
 ]
 where
   setShowContent arg = return $ arg { showContent = True }
   setShowResult  arg = return $ arg { showResult  = True }
   setShowFail    arg = return $ arg { showFail    = True }

showUsage :: IO ()
showUsage = do
  prog <- getProgName
  hPutStrLn stderr ("Usage: " ++ prog ++ " [--show-content] [--show-result]")
  exitFailure

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The tests...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

infix 1 @!?

-- | Boolean false assertion: Expect that the result of the test will be 'False'.
(@!?) :: (AssertionPredicable t) =>
         t
      -> String
      -> Assertion
assrt @!? msg = assertionPredicate assrt >>= (\b -> when b (assertFailure msg))

mkYAMLTests :: TestArgs -> IO Test
mkYAMLTests opts =
  return $ test [ "origin"   ~: test [ "bad origin (1)"    ~: badOrigin00 opts       @!? "bad origin (1) succeeded."
                                     , "bad origin (2)"    ~: badOrigin01 opts       @!? "bad origin (2) succeeded."
                                     , "bad oriign (3)"    ~: badOrigin02 opts       @!? "bad origin (3) succeeded."
                                     , "origin+end"        ~: goodGuidance opts      @?  "origin+end failed."
                                     , "one section"       ~: oneSection opts        @?  "one section failed."
                                     ]
                , "equates"  ~: test [ "valid equate"       ~: validEquate opts      @?  "valid equate failed."
                                     , "invalid equate (1)" ~: invalidEquate00 opts  @!? "invalid equate (1) succeeded."
                                     , "invalid equate (2)" ~: invalidEquate01 opts  @!? "invalid equate (2) succeeded."
                                     , "invalid hex value"  ~: invalidHex opts       @!? "invalid hex succeeded."
                                     , "out of range value" ~: invalidValue opts     @!? "invalid value succeeded."
                                     , "missing equ name"   ~: missingEquName opts   @!? "missing equate name succeeded."
                                     ]
                , "disasm"   ~: test [ "valid disasm"       ~: validDisasm opts      @?  "disasm failed."
                                     ]
                , "bytes"    ~: test [ "valid bytes"        ~: validBytes opts       @?  "bytes directive failed."
                                     ]
                , "ascii"    ~: test [ "valid ascii"        ~: validAscii opts       @? "ascii directive failed."
                                     ]
                , "highbits" ~: test [ "valid highbits"     ~: validHighBits opts    @? "highbits directive failed."
                                     ]
                , "symbols"  ~: test [ "known symbols"      ~: knownSymbols opts     @? "known symbols failed."
                                     ]
                ]

doYAMLTest :: TestArgs -> ByteString -> IO Bool
doYAMLTest opts testString = do
  when (showContent opts)
   (do
    putStrLn "YAML Test Content:"
    putStrLn "~~~~"
    putStrLn (T.unpack . E.decodeUtf8 . S.concat . toChunks $ testString)
    putStrLn "~~~~")
  let handleErr err = (when (showFail opts) . putStrLn . Y.prettyPrintParseException $ err)
                      >> return False
      handleRes res = (when (showResult opts) . print $ res)
                      >> return True
  either handleErr handleRes (yamlStringGuidance testString)

badOrigin00 :: TestArgs -> IO Bool
badOrigin00 opts = doYAMLTest opts [r|origin: not an origin|]

badOrigin01 :: TestArgs -> IO Bool
badOrigin01 opts = doYAMLTest opts [r|---
origin:
    key1: val1
...
|]

badOrigin02 :: TestArgs -> IO Bool
badOrigin02 opts = doYAMLTest opts [r|---
origin:
    origin: $
...
|]

goodGuidance :: TestArgs -> IO Bool
goodGuidance opts = doYAMLTest opts [r|
origin: 0x2e00
end: 0x2fff
|]

oneSection :: TestArgs -> IO Bool
oneSection opts = doYAMLTest opts [r|
origin: 0x2e00
end: 0x2fff
section:
  first_section:
    - comment: |
        This is a multiline comment.
        Fun should ensue!
|]

validEquate :: TestArgs -> IO Bool
validEquate opts = doYAMLTest opts [r|---
origin: 0x0000
end:    0x0001
section:
  valid_equate:
    - comment: |
        multiline comment
        comment line 2
    - equate:
        name: RST08VEC
        value: 0x4000
    - comment: RST10 vector - this is a JP instruction
    - equate: [RST10VEC, 0x4003]
      #  name: RST10VEC
      #  value: 0x4003
...
|]

-- | Invalid equate name
invalidEquate00 :: TestArgs -> IO Bool
invalidEquate00 opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  foo:
    - equate:
        name: 0RST10VEC
        value: 0x4003
|]

-- | Invalid equate name
invalidEquate01 :: TestArgs -> IO Bool
invalidEquate01 opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  foo:
    - equate:[0RST10VEC, 0x4003]
|]


-- | Invalid equate name
invalidHex :: TestArgs -> IO Bool
invalidHex opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  invalid_equate:
    - equate:
        name:  RST10VEC
        value: 0xg
|]

-- | Invalid value
invalidValue :: TestArgs -> IO Bool
invalidValue opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  invalid_value:
    - equate:
        name:  RST10VEC
        value: 65536
|]

-- | Missing equate name
missingEquName :: TestArgs -> IO Bool
missingEquName opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  missing_equate:
    - equate:
        value: 65536
|]

-- | Valid disasm directive
validDisasm :: TestArgs -> IO Bool
validDisasm opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  valid_disasm:
    - disasm:
        nbytes: 0x0a26
        addr: 0x25d9
|]

-- | Valid bytes directive
validBytes :: TestArgs -> IO Bool
validBytes opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  valid_bytes:
    - bytes:
        nbytes: 0x0010
        addr: 0x0050
|]

-- | Valid ASCII directive
validAscii :: TestArgs -> IO Bool
validAscii opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  valid_ascii:
    - ascii:
        addr: 0x0105
        nBytes: 0x000b
|]

-- | Valid highbits directive
validHighBits :: TestArgs -> IO Bool
validHighBits opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  valid_highbits:
    - highbits:
        addr: 0x1650
        nbytes: 0x01d0
|]

knownSymbols :: TestArgs -> IO Bool
knownSymbols opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
  known_symbols:
    - symbols:
        foo: 0x4000
        bar: 0x4001
  |]
