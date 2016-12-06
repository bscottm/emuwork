{- | 'MemorySystem' exercise module -}

module Main (main) where

import           Control.Monad
import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as DVU
import           Data.Word
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO (stderr, hPutStrLn)
import           Test.HUnit
import qualified Data.IntervalMap.Interval as I

import Debug.Trace

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
                                   , "mkROMRegion2"   ~: test_mkROMRegion2 opts    @? "mkROMRegion2 failed."
                                   , "mkRAMRegion3"   ~: test_mkRAMRegion3 opts    @? "mkRAMRegion3 failed."
                                   ]
                , "read"   ~: test [ "read1"          ~: test_read1 opts           @? "read1 failed"
                                   ]
                , "patch"  ~: test [ "patch01"        ~: test_patch01 opts         @? "patch01 failed."
                                   , "patch02"        ~: test_patch02 opts         @? "patch02 failed."
                                   , "patch03"        ~: test_patch03 opts         @? "patch03 failed."
                                   ]
                ]

test_mkROMRegion :: TestArgs -> IO Bool
test_mkROMRegion _args =
  let img   = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys  = M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8
      rlist = map fst (M.regionList msys)
  in  return (M.countRegions msys == 1 && rlist == [I.IntervalCO 0 4096])

test_mkROMRegion2 :: TestArgs -> IO Bool
test_mkROMRegion2 _args =
  let img   = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys  = M.mkROMRegion 4096 img (M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8)
      rlist = map fst (M.regionList msys)
  in  return (M.countRegions msys == 2 && rlist == [I.IntervalCO 0 4096, I.IntervalCO 4096 8192])

test_mkRAMRegion3 :: TestArgs -> IO Bool
test_mkRAMRegion3 _args =
  let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 M.initialMemorySystem :: M.MemorySystem Word16 Word8)
      rlist = map fst (M.regionList msys)
  in  return (M.countRegions msys == 2 && rlist == [I.IntervalCO 0 0x1000, I.IntervalCO 0x1400 0x2400])

test_read1 :: TestArgs -> IO Bool
test_read1 _opts =
  let img   = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys  = M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8
  in  return (M.mRead msys 1 == 1)

test_patch01 :: TestArgs -> IO Bool
test_patch01 args =
  let img    = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys   = M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8
      pvec   = [ 0x11, 0x22, 0x33, 0x44, 0x55, 0x66]
      msys'  = M.mPatch 14 (DVU.fromList pvec) msys
      cmpvec = [ 0x0c, 0x0d ] ++ pvec ++ [ 0x14, 0x15 ]
      memvec = DVU.toList (M.mReadN msys' 12 (length cmpvec))
  in  do
        when (showResult args) (hPutStrLn stderr ("memvec: " ++ show memvec)
                                >> hPutStrLn stderr ("cmpvec: " ++ show cmpvec))
        return (memvec == cmpvec)

test_patch02 :: TestArgs -> IO Bool
test_patch02 args =
  let msys   = M.mkRAMRegion 20 4 (M.mkRAMRegion 0 16 M.initialMemorySystem :: M.MemorySystem Word16 Word8)
      pvec   = [ 0x11, 0x22 ] ++ replicate 4 0  ++ [ 0x33, 0x44, 0x55, 0x66 ]
      msys'  = M.mPatch 14 (DVU.fromList pvec) msys
      cmpvec = [ 0x00, 0x00 ] ++ pvec
      memvec = DVU.toList (M.mReadN msys' 12 (length cmpvec))
  in  do
        when (showResult args) (hPutStrLn stderr ("memvec: " ++ show memvec)
                                >> hPutStrLn stderr ("cmpvec: " ++ show cmpvec))
        return (memvec == cmpvec)

test_patch03 :: TestArgs -> IO Bool
test_patch03 args =
  let msys   = M.mkRAMRegion 20 4 (M.mkRAMRegion 0 16 M.initialMemorySystem :: M.MemorySystem Word16 Word8)
      pvec   = [ 0x11, 0x22 ] ++ replicate 4 0  ++ [ 0x33, 0x44, 0x55, 0x66 ]
      msys'  = M.mPatch 14 (DVU.fromList pvec) msys
      cmpvec = [ 0x00, 0x00 ] ++ pvec ++ [ 0x00, 0x00, 0x00 ]
      memvec = DVU.toList (M.mReadN msys' 12 (length cmpvec))
  in  do
        when (showResult args) (hPutStrLn stderr ("memvec: " ++ show memvec)
                                >> hPutStrLn stderr ("cmpvec: " ++ show cmpvec))
        return (memvec == cmpvec)
