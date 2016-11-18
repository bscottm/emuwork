{- | 'MemorySystem' exercise module -}

module Main (main) where

import           Control.Monad
import qualified Data.Foldable as Foldable
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as DVU
import           Data.Word
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO (stdout, stderr, hPutStrLn)
import           Test.HUnit

import qualified Machine.MemorySystem as M

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
  getOpt RequireOrder testOptions <$> getArgs
  >>= (\(optsActions, rest, errs) ->
        unless (null errs)
          (mapM_ (hPutStrLn stderr) errs
            >> showUsage
            >> exitFailure)
         >> Foldable.foldl (>>=) (return mkTestArgs) optsActions
         >>= (\options ->
                if null rest
                then
                  mkMsysTests options
                  >>= (\tests ->
                         do { (counts', _) <- performTest reportStart reportError reportFailure () tests
                            ; putStrLn ""
                            ; putStrLn "Summary:"
                            ; putStrLn (showCounts counts')
                            ; if errors counts' == 0 || failures counts' == 0
                              then exitSuccess
                              else exitFailure
                            })
                else
                  showUsage))
  where
    reportStart ss _us = TIO.putStrLn (testPath (path ss))
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
           path' = testPath (path ss)
    testPath [] = T.empty
    testPath nodes = T.concat ["++ ", T.intercalate ":" (map showNode (reverse (filter onlyLabels nodes)))]
     where onlyLabels (Label _) = True
           onlyLabels _         = False
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
predTest @!? msg = assertionPredicate predTest >>= (\b -> when b (assertFailure msg))

mkMsysTests :: TestArgs -> IO Test
mkMsysTests opts =
  return $ test [ "msys"   ~: test [ "mkROMRegion"    ~: test_mkROMRegion opts     @? "mkROMRegion failed."
                                   ]
                   ]

test_mkROMRegion :: TestArgs -> IO Bool
test_mkROMRegion _args =
  let img   = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word16
      msys  = M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word16
  in   return True
