{- | 'MemorySystem' exercise module -}

module Main (main) where

import           Control.Monad
import qualified Data.Foldable             as Fold (foldl, foldl')
import qualified Data.IntervalMap.Interval as I
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Vector.Unboxed       (Vector, (!))
import qualified Data.Vector.Unboxed       as DVU
import           Data.Word
import           Prelude                   hiding ((!))
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO                 (hPutStrLn, stdout, stderr, hFlush)
import           System.Random             (StdGen, getStdGen, randomRs)
import           Test.HUnit

import           Debug.Trace

import qualified Machine.MemorySystem      as M
import           Machine.Utils             (as0xHexS)

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
         >> Fold.foldl (>>=) (return mkTestArgs) optsActions
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
    reportStart ss _us = TIO.putStrLn (testPath (path ss)) >> hFlush stdout
    reportError   = reportProblem "Error:"   "Error in:   "
    reportFailure = reportProblem "Failure:" "Failure in: "
    reportProblem p0 p1 _loc msg ss _us =
       let line  = T.concat [ "### "
                            , kind
                            , path'
                            , "\n"
                            , T.pack msg
                            ]
           kind  = T.pack (if T.null path' then p0 else p1)
           path' = testPath (path ss)
       in  TIO.putStrLn line >> hFlush stdout
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
  , rng         :: IO StdGen
  }

mkTestArgs :: TestArgs
mkTestArgs = TestArgs
                { showContent = False
                , showResult  = False
                , showFail    = False
                , rng         = getStdGen
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

-- | Boolean false assertion: Expect that the result of the test will be 'False' (i.e., expected failure.)
(@!?) :: (AssertionPredicable t) =>
         t
      -> String
      -> Assertion
predTest @!? msg = assertionPredicate predTest >>= (\b -> when b (assertFailure msg))

mkMsysTests :: TestArgs -> IO Test
mkMsysTests opts =
  return $ test [ "msys"    ~: test [ "mkROMRegion"       ~: test_mkROMRegion opts       @? "mkROMRegion failed."
                                    , "mkROMRegion2"      ~: test_mkROMRegion2 opts      @? "mkROMRegion2 failed."
                                    , "mkRAMRegion3"      ~: test_mkRAMRegion3 opts      @? "mkRAMRegion3 failed."
                                    ]
                , "readROM" ~: test [ "ROMread1"          ~: test_ROMread1 opts          @? "ROMread1 failed."
                                    , "ROMsequential"     ~: test_ROMsequential opts     @? "ROMsequential failed."
                                    , "ROMrandom"         ~: test_ROMrandom opts         @? "ROMrandom failed."
                                    , "randROMread1"      ~: test_randROMread1 opts      @? "randROMread1 failed."
                                    , "randROMsequential" ~: test_randROMsequential opts @? "randROMsequential failed."
                                    , "randROMrandom"     ~: test_randROMrandom opts     @? "randROMrandom failed."
                                    , "gapROMBefore"      ~: test_gapROMBefore opts      @? "gapROMBefore failed."
                                    , "gapROMTotal"       ~: test_gapROMTotal opts       @? "gapROMTotal failed."
                                    -- , "gapSlidingWindow"  ~: test_gapWindows opts        @? "gapSlidingWindow failed."
                                    ]
                , "patch"   ~: test [ "patchROM01"        ~: test_patchROM01 opts        @? "patch01 failed."
                                    , "patchROM02"        ~: test_patchROM02 opts        @? "patch02 failed."
                                    , "patchROM03"        ~: test_patchROM03 opts        @? "patch03 failed."
                                    ]
                , "write"   ~: test [ "RAMwrite1"         ~: test_RAMwrite1 opts         @? "RAMwrite1 failed."
                                    , "RAMwrite5"         ~: test_RAMwrite5 opts         @? "RAMwrite5 failed."
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

-- | Simple ROM
simpleROMMsys  :: M.MemorySystem Word16 Word8
simpleROMMsys = M.mkROMRegion 0 readROMImg M.initialMemorySystem

readROMImg :: Vector Word8
readROMImg  = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256))

-- | Random value ROM
randROMMsys :: Vector Word8 -> M.MemorySystem Word16 Word8
randROMMsys img = M.mkROMRegion 0 img M.initialMemorySystem

randROMImg :: IO StdGen -> IO (Vector Word8)
randROMImg  gen =
  do
    stdGen <- gen
    return (DVU.fromList (take 4096 (randomRs (0, 255) stdGen)))

test_ROMread1 :: TestArgs -> IO Bool
test_ROMread1 _opts = return (M.mRead simpleROMMsys 1 == 1)

test_ROMsequential :: TestArgs -> IO Bool
test_ROMsequential _opts = return (and [M.mRead simpleROMMsys (fromIntegral i) == fromIntegral (i `mod` 256)
                                       | i <- [0..DVU.length readROMImg - 1]
                                       ])

test_ROMrandom :: TestArgs -> IO Bool
test_ROMrandom opts =
  do
    stdGen <- rng opts
    let idxRange = DVU.length readROMImg
        idxs     = take (idxRange * 4) (randomRs (0, idxRange - 1) stdGen)
    return (and [M.mRead simpleROMMsys (fromIntegral i) == fromIntegral (i  `mod` 256) | i <- idxs])

test_randROMread1 :: TestArgs -> IO Bool
test_randROMread1 opts =
  do
    img <- randROMImg (rng opts)
    let msys = randROMMsys img
    return (M.mRead msys 1 == img ! 1)

test_randROMsequential :: TestArgs -> IO Bool
test_randROMsequential opts =
  do
    img <- randROMImg (rng opts)
    let msys = randROMMsys img
    return (and [M.mRead msys (fromIntegral i) == img ! i | i <- [0..DVU.length readROMImg - 1]])

test_randROMrandom :: TestArgs -> IO Bool
test_randROMrandom opts =
  do
    stdGen <- rng opts
    img    <- randROMImg (rng opts)
    let msys     = randROMMsys img
    let idxRange = DVU.length img
        idxs     = take (idxRange * 4) (randomRs (0, idxRange - 1) stdGen)
    return (and [M.mRead msys (fromIntegral i) == img ! i | i <- idxs])

-- | start address for the gapped ROM region tests. this is a prime number instead of a power-of-2.
gapROM_addr_1, gapROM_addr_2 :: Word16
gapROM_addr_1 = 2719
gapROM_addr_2 = 3907

gapROM_len_1, gapROM_len_2 :: Int
gapROM_len_1  = 1129
gapROM_len_2  = 4027

gapROMGap, gapROMTotal :: Int
gapROMGap     = fromIntegral gapROM_addr_2 - fromIntegral gapROM_addr_1 - gapROM_len_1
gapROMTotal   = fromIntegral gapROM_len_1 + fromIntegral gapROM_len_2 + gapROMGap

gapROMImg_1, gapROMImg_2 :: Vector Word8
gapROMImg_1   = DVU.generate gapROM_len_1 (\x -> fromIntegral (x `mod` 256))
gapROMImg_2   = DVU.generate gapROM_len_2 (\x -> fromIntegral (x `mod` 256))

gapROMMsys :: M.MemorySystem Word16 Word8
gapROMMsys    = M.mkROMRegion gapROM_addr_2 gapROMImg_2 (M.mkROMRegion gapROM_addr_1 gapROMImg_1 M.initialMemorySystem)

test_gapROMTotal :: TestArgs -> IO Bool
test_gapROMTotal opts =
  do
    let memvec = M.mReadN gapROMMsys gapROM_addr_1 gapROMTotal
        cmpvec = DVU.concat [gapROMImg_1, DVU.replicate gapROMGap 0, gapROMImg_2]
    when (showResult opts) (hPutStrLn stderr "gapROMTotal:"
                            >> hPutStrLn stderr ("length memvec: " ++ show (DVU.length memvec))
                            >> hPutStrLn stderr ("length cmpvec: " ++ show (DVU.length cmpvec)))
    return (memvec == cmpvec)

test_gapROMBefore :: TestArgs -> IO Bool
test_gapROMBefore opts =
  do
    let memvec = DVU.toList (M.mReadN gapROMMsys (gapROM_addr_1 - 17) (17 + 19))
    let cmpvec = replicate 17 0 ++ DVU.toList (DVU.slice 0 19 gapROMImg_1)
    when (showResult opts) (hPutStrLn stderr "gapROMBefore:"
                            >> hPutStrLn stderr ("memvec: " ++ as0xHexS memvec)
                            >> hPutStrLn stderr ("cmpvec: " ++ as0xHexS cmpvec))
    return (memvec == cmpvec)

test_gapWindows :: TestArgs -> IO Bool
test_gapWindows _opts =
  let totalGapVec = DVU.concat [ gapROMImg_1
                               , DVU.replicate (fromIntegral gapROM_addr_2 - fromIntegral gapROM_addr_1 - gapROM_len_1) 0
                               , gapROMImg_2
                               ]
      totalLen    = DVU.length totalGapVec
      outer i lim
        | i == lim
        = return True
        | otherwise
        = inner (totalLen - fromIntegral i) 0 i
          >>= (\b -> if b
                     then outer (i + 1) lim
                     else return False)
      inner toRead offs lim
        {-  | trace ("inner " ++ as0xHexS offs ++ " toRead " ++ show toRead) False = undefined -}
        | offs == lim
        = return True
        | otherwise
        = let memvec = M.mReadN gapROMMsys (gapROM_addr_1 + offs) toRead
              cmpvec = DVU.slice (fromIntegral offs) toRead totalGapVec
          in if memvec == cmpvec
             then inner toRead (offs + 1) lim
             else hPutStrLn stderr ("gapWindows/inner@" ++ as0xHexS offs ++
                                    ", toRead = " ++ show toRead ++
                                    " cmpvec " ++ show (DVU.length cmpvec) ++
                                    " memvec " ++ show (DVU.length memvec))
                  >> hPutStrLn stderr (as0xHexS (DVU.toList memvec))
                  >> hPutStrLn stderr (as0xHexS (DVU.toList cmpvec))
                  >> return False
  in outer 0 (fromIntegral (totalLen - 1))

test_patchROM01 :: TestArgs -> IO Bool
test_patchROM01 args =
  let img    = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys   = M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8
      pvec   = [ 0x11, 0x22, 0x33, 0x44, 0x55, 0x66]
      msys'  = M.mPatch 14 (DVU.fromList pvec) msys
      cmpvec = [ 0x0c, 0x0d ] ++ pvec ++ [ 0x14, 0x15 ]
      memvec = DVU.toList (M.mReadN msys' 12 (length cmpvec))
  in  do
        when (showResult args) (hPutStrLn stderr ("memvec: " ++ as0xHexS memvec)
                                >> hPutStrLn stderr ("cmpvec: " ++ as0xHexS cmpvec))
        return (memvec == cmpvec)

patchROMMsys_1 :: M.MemorySystem Word16 Word8
patchROMMsys_1 = M.mkRAMRegion 20 4 (M.mkRAMRegion 0 16 M.initialMemorySystem)

test_patchROM02 :: TestArgs -> IO Bool
test_patchROM02 args =
  let pvec   = [ 0x11, 0x22 ] ++ replicate 4 0  ++ [ 0x33, 0x44, 0x55, 0x66 ]
      msys'  = M.mPatch 14 (DVU.fromList pvec) patchROMMsys_1
      cmpvec = [ 0x00, 0x00 ] ++ pvec
      memvec = DVU.toList (M.mReadN msys' 12 (length cmpvec))
  in  do
        when (showResult args) (hPutStrLn stderr ("memvec: " ++ as0xHexS memvec)
                                >> hPutStrLn stderr ("cmpvec: " ++ as0xHexS cmpvec))
        return (memvec == cmpvec)

test_patchROM03 :: TestArgs -> IO Bool
test_patchROM03 args =
  let pvec   = [ 0x11, 0x22 ] ++ replicate 4 0  ++ [ 0x33, 0x44, 0x55, 0x66 ]
      msys'  = M.mPatch 14 (DVU.fromList pvec) patchROMMsys_1
      cmpvec = [ 0x00, 0x00 ] ++ pvec ++ [ 0x00, 0x00, 0x00 ]
      memvec = DVU.toList (M.mReadN msys' 12 (length cmpvec))
  in  do
        when (showResult args) (hPutStrLn stderr ("memvec: " ++ as0xHexS memvec)
                                >> hPutStrLn stderr ("cmpvec: " ++ as0xHexS cmpvec))
        return (memvec == cmpvec)

writeRAMPatch_1 :: Vector Word8
writeRAMPatch_1 = DVU.generate (12 * 1024) (\x -> fromIntegral (x `mod` 256))

writeRAMMsys :: M.MemorySystem Word16 Word8
writeRAMMsys    = M.mPatch 0 writeRAMPatch_1 (M.mkRAMRegion 0 (12 * 1024) M.initialMemorySystem)

test_RAMwrite1 :: TestArgs -> IO Bool
test_RAMwrite1 _opts =
  let msys = M.mWrite 0 0xff writeRAMMsys
  in  return (M.mRead msys 0 == 0xff)

test_RAMwrite5 :: TestArgs -> IO Bool
test_RAMwrite5 _opts =
  let writeRAM m (addr, val) = M.mWrite addr val m
      msys = Fold.foldl writeRAM writeRAMMsys [(0, 0xff), (3, 0xaa), (2, 0xbb), (1, 0xcc), (4, 0x55)]
  in  return (M.mReadN msys 0 7 == DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55, 0x05, 0x06])
