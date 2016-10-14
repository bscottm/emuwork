{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import           Disasm.Guidance
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO (stdout, stderr, hPutStrLn)
import           Test.HUnit
import           Text.RawString.QQ

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
  getArgs
  >>= return . getOpt RequireOrder testOptions
  >>= (\(optsActions, rest, errs) ->
         (unless (null errs) $ mapM_ (hPutStrLn stderr) errs
           >> showUsage
           >> exitFailure)
         >> Foldable.foldl' (>>=) (return mkTestArgs) optsActions
         >>= (\options ->
                if null rest
                then
                  mkYAMLTests options
                  >>= (\tests ->
                         do { (counts', _) <- performTest reportStart reportError reportFailure () tests
                            ; putStrLn ""
                            ; putStrLn "Summary:"
                            ; putStrLn (showCounts counts')
                            ; if (errors counts') == 0 || (failures counts') == 0
                              then exitSuccess
                              else exitFailure
                            })
                else
                  showUsage))
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

data TestArgs =
  TestArgs
  { showContent :: Bool
  , showResult  :: Bool
  }

mkTestArgs :: TestArgs
mkTestArgs = TestArgs
                { showContent = False
                , showResult  = False
                }

testOptions :: [OptDescr (TestArgs -> IO TestArgs)]
testOptions =
 [ Option [] ["show-content"] (NoArg setShowContent) "Show test string's contents"
 , Option [] ["show-result"]  (NoArg setShowResult)  "Show test's result"
 ]
 where
   setShowContent arg = return $ arg { showContent = True }
   setShowResult  arg = return $ arg { showResult  = True }
   
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
pred @!? msg = assertionPredicate pred >>= (\b -> when b (assertFailure msg))

mkYAMLTests :: TestArgs -> IO Test
mkYAMLTests opts =
  let sc = showContent opts
      sr = showResult  opts
  in return $ test [ "test1" ~: (test1 opts) @?  "test1 failed."
                   , "equates" ~: test [ "valid equate"      ~: (validEquate opts)   @?  "valid equate failed."
                                       , "invalid equate"    ~: (invalidEquate opts) @!? "invalid equate succeeded."
                                       , "invalid hex value" ~: (invalidHex opts)    @!? "invalid hex succeeded."
                                       ]
                   ]

doYAMLTest :: TestArgs -> ByteString -> IO Bool
doYAMLTest opts testString =
  (when (showContent opts) $
   do
     putStrLn "YAML Test Content:"
     putStrLn "~~~~"
     putStrLn $ (T.unpack . decodeUtf8) testString
     putStrLn "~~~~")
  >> (case Y.decodeEither' testString :: Either Y.ParseException [Guidance] of
        Left  err ->
          do
            when (showResult opts) $ putStrLn ("Decode failed: " ++ (show err))
            return False
        Right res ->
          do
            when (showResult opts) $ putStrLn (show res)
            return True)

test1 :: TestArgs -> IO Bool
test1 opts = doYAMLTest opts [r|
- origin: 0x0
- comment: Restart vector redirections. These are 'JP' instructions
- comment: |
    multiline comment
    comment line 2
|]

validEquate :: TestArgs -> IO Bool
validEquate opts = doYAMLTest opts [r|- origin: 0x0
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
|]

-- | Invalid equate name
invalidEquate :: TestArgs -> IO Bool
invalidEquate opts = doYAMLTest opts [r|
- equate:
    name: 0RST10VEC
    value: 0x4003
|]


-- | Invalid equate name
invalidHex :: TestArgs -> IO Bool
invalidHex opts = doYAMLTest opts [r|
- equate:
    name:  RST10VEC
    value: 0xg
|]