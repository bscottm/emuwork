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

mkYAMLTests :: TestArgs -> IO Test
mkYAMLTests opts =
  let sc = showContent opts
      sr = showResult  opts
  in return $ test [ "test1" ~: (test1 sc sr) @? "test1 failed."
                   , "test2" ~: (test2 sc sr) @? "test2 failed."
                   ]

doYAMLTest :: Bool -> Bool -> ByteString -> IO Bool
doYAMLTest showContent showResult testString =
  (when showContent $
   do
     putStrLn "YAML Test Content:"
     putStrLn "~~~~"
     putStrLn $ (T.unpack . decodeUtf8) testString
     putStrLn "~~~~")
  >> (case Y.decodeEither' testString :: Either Y.ParseException [Guidance] of
        Left  err ->
          do
            putStrLn ("Decode failed: " ++ (show err))
            return False
        Right res ->
          do
            when showResult $ putStrLn (show res)
            return True)

test1 :: Bool -> Bool -> IO Bool
test1 sc sr = doYAMLTest sc sr [r|
- origin: 0x0
- comment: Restart vector redirections. These are 'JP' instructions
- comment: |
    multiline comment
    comment line 2
|]

test2 :: Bool -> Bool -> IO Bool
test2 sc sr = doYAMLTest sc sr [r|- origin: 0x0
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