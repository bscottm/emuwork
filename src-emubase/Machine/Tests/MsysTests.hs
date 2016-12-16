{- | 'MemorySystem' exercise module -}

module Main (main) where

import qualified Data.Foldable                        as Fold (foldl)
import qualified Data.IntervalMap.Interval            as I
import           Data.List                            (elemIndices)
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          (mempty)
import           Data.Vector.Unboxed                  (Vector, (!))
import qualified Data.Vector.Unboxed                  as DVU
import           Data.Word
import           System.IO                            (hPutStrLn, stderr)
import           System.Random                        (getStdGen, randomRs)
import           Test.Framework                       (Test, defaultMain, plusTestOptions, testGroup)
import           Test.Framework.Options               (TestOptions' (..))
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertBool, assertFailure)
import           Test.QuickCheck                      (Property, choose, forAll)

-- import           Debug.Trace

import qualified Machine.MemorySystem                 as M
import           Machine.Utils                        (as0xHexS)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main = do
  stdGen  <- getStdGen
  -- Generate the various random vectors we need to test:
  let options = TestArgs { randROMImg = DVU.fromList (take 4096 (randomRs (0, 0xff) stdGen)) }
  defaultMain (mkMsysTests options)

data TestArgs =
  TestArgs
  { randROMImg  :: Vector Word8
  }

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The tests...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

mkMsysTests :: TestArgs -> [Test]
mkMsysTests opts =
  [ testGroup "msys regions"
    [ testCase "mkROMRegion"       (test_mkROMRegion opts)
    , testCase "mkROMRegion2"      (test_mkROMRegion2 opts)
    , testCase "mkRAMRegion3"      (test_mkRAMRegion3 opts)
    ]
  , testGroup "readROM"
    [ testCase "ROMread1"          (test_ROMread1 opts)
    , testCase "ROMsequential"     (test_ROMsequential opts)
    , plusTestOptions propROMRandom_opts (testProperty "ROMrandom" prop_ROMrandom)
    , testCase "randROMread1"      (test_randROMread1 opts)
    , testCase "randROMsequential" (test_randROMsequential opts)
    , plusTestOptions propRandROMRandom_opts (testProperty "randROMrandom" (prop_randROMrandom opts))
    , testCase "gapROMBefore"      (test_gapROMBefore opts)
    , testCase "gapROMTotal"       (test_gapROMTotal opts)
    -- , testCase "gapSlidingWindow"  (test_gapWindows opts)
    ]
  , testGroup "patch"
    [ testCase "patchROM01"        (test_patchROM01 opts)
    , testCase "patchROM02"        (test_patchROM02 opts)
    , testCase "patchROM03"        (test_patchROM03 opts)
    ]
  , testGroup "write"
    [ testCase "RAMwrite1"         (test_RAMwrite1 opts)
    , testCase "RAMwrite5"         (test_RAMwrite5 opts)
    ]
  ]
  where
    -- 'mempty' is really the TestOptions record instantiated with all of the
    -- members set to Nothing.
    propROMRandom_opts = mempty { topt_maximum_generated_tests = Just propROMRandom_tests
                                , topt_maximum_unsuitable_generated_tests = Just propROMRandom_tests
                                }
    propROMRandom_tests = DVU.length readROMImg * 4
    propRandROMRandom_opts = mempty { topt_maximum_generated_tests = Just propRandROMRandom_tests
                                    , topt_maximum_unsuitable_generated_tests = Just propRandROMRandom_tests
                                    }
    propRandROMRandom_tests = DVU.length (randROMImg opts) * 4

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- Compare two vectors
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

compareVectors :: Vector Word8
               -> Vector Word8
               -> String
               -> String
               -> Maybe String
compareVectors l r lName rName
  | DVU.length l < DVU.length r
  = Just (lName ++ " length (" ++ show (DVU.length l) ++ ") less than " ++ rName ++ "(" ++ show (DVU.length r) ++ ")")
  | DVU.length l > DVU.length r
  = Just (lName ++ " length (" ++ show (DVU.length l) ++ ") greater than " ++ rName ++ "(" ++ show (DVU.length r) ++ ")")
  | DVU.null diffIdxs
  = Nothing
  | otherwise
  = Just (lName ++ " differs: (idx, expected, actual): " ++ show idxValues)
  where
    diffIdxs  = DVU.elemIndices False (DVU.imap (\idx v -> v == r ! idx) l)
    idxValues = DVU.toList (DVU.map (\i -> (i, r ! i, l ! i)) (DVU.take 10 diffIdxs))

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- The tests...
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

test_mkROMRegion :: TestArgs -> Assertion
test_mkROMRegion _args =
  let img      = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys     = M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8
      rlist    = map fst (M.regionList msys)
      nRegions = M.countRegions msys == 1
      mRegions = rlist == [I.IntervalCO 0 4096]
  in  assertBool (if   not nRegions
                  then "Expected 1 region, got " ++ show (M.countRegions msys)
                  else "Expected 4096 Word8 region, got " ++ show rlist)
                 (nRegions && mRegions)

test_mkROMRegion2 :: TestArgs -> Assertion
test_mkROMRegion2 _args =
  let img   = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys  = M.mkROMRegion 4096 img (M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8)
      rlist = map fst (M.regionList msys)
      nRegions = M.countRegions msys == 2
      mRegions = rlist == [I.IntervalCO 0 4096, I.IntervalCO 4096 8192]
  in  assertBool (if   not nRegions
                  then "Expected 2 regions, got " ++ show (M.countRegions msys)
                  else "Expected 2 intervals, got " ++ show rlist)
                 (nRegions && mRegions)

test_mkRAMRegion3 :: TestArgs -> Assertion
test_mkRAMRegion3 _args =
  let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 M.initialMemorySystem :: M.MemorySystem Word16 Word8)
      rlist = map fst (M.regionList msys)
  in  assertBool "Expected 2 RAM regions"
                 (M.countRegions msys == 2 && rlist == [I.IntervalCO 0 0x1000, I.IntervalCO 0x1400 0x2400])

-- | Simple ROM
simpleROMMsys  :: M.MemorySystem Word16 Word8
simpleROMMsys = M.mkROMRegion 0 readROMImg M.initialMemorySystem

readROMImg :: Vector Word8
readROMImg  = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256))

-- | Random value ROM
randROMMsys :: Vector Word8 -> M.MemorySystem Word16 Word8
randROMMsys img = M.mkROMRegion 0 img M.initialMemorySystem

test_ROMread1 :: TestArgs -> Assertion
test_ROMread1 _opts = assertBool "Read byte from ROM, 1 != 1" (M.mRead simpleROMMsys 1 == 1)

test_ROMsequential :: TestArgs -> Assertion
test_ROMsequential _opts =
  let mReads = [M.mRead simpleROMMsys (fromIntegral i) == fromIntegral (i `mod` 256) | i <- [0..DVU.length readROMImg - 1]]
      wrong  = elemIndices False mReads
  in  assertBool ("Mismatched reads at indices: " ++ show wrong) (and mReads)

prop_ROMrandom :: Property
prop_ROMrandom =
  let readROM idx = M.mRead simpleROMMsys (fromIntegral idx) == fromIntegral (idx  `mod` 256)
  in  forAll (choose (0, DVU.length readROMImg - 1)) readROM

test_randROMread1 :: TestArgs -> Assertion
test_randROMread1 opts =
  let img  = randROMImg opts
      msys = randROMMsys img
      val  = M.mRead msys 1
  in assertBool ("Read byte 1 from random ROM image: expected 1, got " ++ show val)
                (val == img ! 1)

test_randROMsequential :: TestArgs -> Assertion
test_randROMsequential opts =
  let img    = randROMImg opts
      msys   = randROMMsys img
      mReads = [M.mRead msys (fromIntegral i) == img ! i | i <- [0..DVU.length readROMImg - 1]]
      wrong  = elemIndices False mReads
  in  assertBool ("Mismatched reads at indices: " ++ show wrong) (and mReads)

prop_randROMrandom :: TestArgs -> Property
prop_randROMrandom opts =
  let img         = randROMImg opts
  in  forAll (choose (0, DVU.length img - 1)) (\idx -> M.mRead (randROMMsys img) (fromIntegral idx) == img ! idx)

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

test_gapROMTotal :: TestArgs -> Assertion
test_gapROMTotal _opts =
  do
    let memvec = M.mReadN gapROMMsys gapROM_addr_1 gapROMTotal
        cmpvec = DVU.concat [gapROMImg_1, DVU.replicate gapROMGap 0, gapROMImg_2]
    assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
               (memvec == cmpvec)

test_gapROMBefore :: TestArgs -> Assertion
test_gapROMBefore _opts =
  do
    let memvec = M.mReadN gapROMMsys (gapROM_addr_1 - 17) (17 + 19)
    let cmpvec = DVU.concat [DVU.replicate 17 0, DVU.slice 0 19 gapROMImg_1]
    assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
               (memvec == cmpvec)

test_gapWindows :: TestArgs -> Assertion
test_gapWindows _opts =
  let totalGapVec = DVU.concat [ gapROMImg_1
                               , DVU.replicate (fromIntegral gapROM_addr_2 - fromIntegral gapROM_addr_1 - gapROM_len_1) 0
                               , gapROMImg_2
                               ]
      totalLen    = DVU.length totalGapVec
      outer i lim
        | i == lim
        = return ()
        | otherwise
        = let toRead = (totalLen - fromIntegral i)
          in  inner toRead 0 i
              >>= (\b -> if b
                         then outer (i + 1) lim
                         else assertFailure ("gapWindows failed at read length " ++ show toRead))
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

test_patchROM01 :: TestArgs -> Assertion
test_patchROM01 _args =
  let img    = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys   = M.mkROMRegion 0 img M.initialMemorySystem :: M.MemorySystem Word16 Word8
      pvec   = DVU.fromList [ 0x11, 0x22, 0x33, 0x44, 0x55, 0x66]
      msys'  = M.mPatch 14 pvec msys
      cmpvec = DVU.concat [ DVU.fromList [0x0c, 0x0d ], pvec, DVU.fromList [ 0x14, 0x15 ]]
      memvec = M.mReadN msys' 12 (DVU.length cmpvec)
  in  assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec)

patchROMMsys_1 :: M.MemorySystem Word16 Word8
patchROMMsys_1 = M.mkRAMRegion 20 4 (M.mkRAMRegion 0 16 M.initialMemorySystem)

test_patchROM02 :: TestArgs -> Assertion
test_patchROM02 _args =
  let pvec   = DVU.concat [ DVU.fromList [0x11, 0x22 ], DVU.replicate 4 0, DVU.fromList [ 0x33, 0x44, 0x55, 0x66 ]]
      msys'  = M.mPatch 14 pvec patchROMMsys_1
      cmpvec = DVU.concat [ DVU.fromList [ 0x00, 0x00 ], pvec ]
      memvec = M.mReadN msys' 12 (DVU.length cmpvec)
  in assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                (memvec == cmpvec)

test_patchROM03 :: TestArgs -> Assertion
test_patchROM03 _args =
  let pvec   = DVU.concat [ DVU.fromList [ 0x11, 0x22 ], DVU.replicate 4 0, DVU.fromList [ 0x33, 0x44, 0x55, 0x66 ]]
      msys'  = M.mPatch 14 pvec patchROMMsys_1
      cmpvec = DVU.concat [ DVU.fromList [ 0x00, 0x00 ], pvec, DVU.fromList [ 0x00, 0x00, 0x00 ]]
      memvec = M.mReadN msys' 12 (DVU.length cmpvec)
  in assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                (memvec == cmpvec)

writeRAMPatch_1 :: Vector Word8
writeRAMPatch_1 = DVU.generate (12 * 1024) (\x -> fromIntegral (x `mod` 256))

writeRAMMsys :: M.MemorySystem Word16 Word8
writeRAMMsys    = M.mPatch 0 writeRAMPatch_1 (M.mkRAMRegion 0 (12 * 1024) M.initialMemorySystem)

test_RAMwrite1 :: TestArgs -> Assertion
test_RAMwrite1 _opts =
  let msys = M.mWrite 0 0xff writeRAMMsys
      mem0 = M.mRead msys 0
  in  assertBool ("Write RAM, expected 0xff, got " ++ as0xHexS mem0)
                 (mem0 == 0xff)

test_RAMwrite5 :: TestArgs -> Assertion
test_RAMwrite5 _opts =
  let writeRAM m (addr, val) = M.mWrite addr val m
      msys = Fold.foldl writeRAM writeRAMMsys [(0, 0xff), (3, 0xaa), (2, 0xbb), (1, 0xcc), (4, 0x55)]
  in  assertBool "write compare mismatch"
                 (M.mReadN msys 0 7 == DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55, 0x05, 0x06])
