{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad
import           Data.ByteString.Lazy (ByteString, toChunks)
import qualified Data.ByteString as S
import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Yaml as Y
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO (stderr, hPutStrLn)
import           Test.HUnit
import           Text.RawString.QQ

import           TRS80.Disasm.Guidance

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
          unless (null errs) (mapM_ (hPutStrLn stderr) errs >> showUsage >> exitFailure)
          >> unless (null rest) showUsage
          >> Foldable.foldl (>>=) (return mkTestArgs) optsActions

    runYAMLTests tests =
      do { (counts', _) <- performTest reportStart reportError reportFailure () tests
        ; putStrLn ""
        ; putStrLn "Summary:"
        ; putStrLn (showCounts counts')
        ; if errors counts' == 0 || failures counts' == 0
          then exitSuccess
          else exitFailure
        }
  
    reportStart ss _us = putStrLn . T.unpack . showPath . path $ ss
    reportError   = reportProblem "Error:"   "Error in:   "
    reportFailure = reportProblem "Failure:" "Failure in: "
    reportProblem p0 p1 _loc msg ss _us = putStrLn line
      where
        line  = concat [ "### "
                       , if null path' then p0 else p1
                       , path'
                       , "\n"
                       , msg
                       ]
        path' = T.unpack. showPath . path $ ss
    showPath [] = T.empty
    showPath nodes = T.concat ["++ ", T.intercalate ":" (map showNode (reverse (filter onlyLabels nodes)))]
     where onlyLabels (Label _)   = True
           onlyLabels _           = False
           showNode (ListItem _n) = T.empty
           showNode (Label label) = T.justifyLeft 15 ' ' (T.pack $ safe label (show label))
           safe s ss = if ':' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s

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
  return $ test [ "origin"   ~: test [ "bad origin (1)"    ~: badOrigin00 opts     @!? "bad origin (1) succeeded."
                                     , "bad origin (2)"    ~: badOrigin01 opts     @!? "bad origin (2) succeeded."
                                     , "origin+end"        ~: goodGuidance opts    @?  "origin+end failed."
                                     , "one section"       ~: oneSection opts      @?  "one section failed."
                                     ]
                , "equates"  ~: test [ "valid equate"       ~: validEquate opts    @?  "valid equate failed."
                                     , "invalid equate"     ~: invalidEquate opts  @!? "invalid equate succeeded."
                                     , "invalid hex value"  ~: invalidHex opts     @!? "invalid hex succeeded."
                                     , "out of range value" ~: invalidValue opts   @!? "invalid value succeeded."
                                     , "missing equ name"   ~: missingEquName opts @!? "missing equate name succeeded."
                                     ]
                , "disasm"   ~: test [ "valid disasm"       ~: validDisasm opts    @?  "disasm failed."
                                     ]
                , "bytes"    ~: test [ "valid bytes"        ~: validBytes opts     @?  "bytes directive failed."
                                     ]
                , "ascii"    ~: test [ "valid ascii"        ~: validAscii opts     @? "ascii directive failed."
                                     ]
                , "highbits" ~: test [ "valid highbits"     ~: validHighBits opts  @? "highbits directive failed."
                                     ]
                , "symbols"  ~: test [ "known symbols"      ~: knownSymbols opts   @? "known symbols failed."
                                     ]
                ]

doYAMLTest :: TestArgs -> ByteString -> IO Bool
doYAMLTest opts testString =
  when (showContent opts) (do
    putStrLn "YAML Test Content:"
    putStrLn "~~~~"
    putStrLn (T.unpack . decodeUtf8 . S.concat . toChunks $ testString)
    putStrLn "~~~~")
  >> (case yamlStringGuidance testString of
        Left  err ->
          do
            when (showFail opts) (putStrLn (Y.prettyPrintParseException err))
            return False
        Right res ->
          do
            when (showResult opts) $ print res
            return True)

badOrigin00 :: TestArgs -> IO Bool
badOrigin00 opts = doYAMLTest opts [r|origin: not an origin|]

badOrigin01 :: TestArgs -> IO Bool
badOrigin01 opts = doYAMLTest opts [r|---
origin:
    key1: val1
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
secton 1:
  - comment: |
      This is a multiline comment.
      Fun should ensue!
|]

validEquate :: TestArgs -> IO Bool
validEquate opts = doYAMLTest opts [r|---
origin: 0x0000
end:    0x0001
section 1:
    - comment: |
        multiline comment
        comment line 2
    - equate:
        name: RST08VEC
        value: 0x4000
    - comment: RST10 vector - this is a JP instruction
    - equate:
        name: RST10VEC
        value: 0x4003
...
|]

-- | Invalid equate name
invalidEquate :: TestArgs -> IO Bool
invalidEquate opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
    - equate:
        name: 0RST10VEC
        value: 0x4003
|]


-- | Invalid equate name
invalidHex :: TestArgs -> IO Bool
invalidHex opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
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
    - equate:
        value: 65536
|]

-- | Valid disasm directive
validDisasm :: TestArgs -> IO Bool
validDisasm opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
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
- highbits:
    addr: 0x1650
    nbytes: 0x01d0
|]

knownSymbols :: TestArgs -> IO Bool
knownSymbols opts = doYAMLTest opts [r|
origin: 0x0000
end:    0x0001
section:
- symbols:
    foo: 0x4000
    bar: 0x4001
|]
