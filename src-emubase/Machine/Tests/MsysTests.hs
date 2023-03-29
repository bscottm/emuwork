{-# LANGUAGE  CPP #-}

{- | 'MemorySystem' exercise module -}

module Main where

import           Control.Arrow                        (first, second)
import           Control.Monad                        (replicateM, unless)
import           Control.Monad.Trans.State.Strict     (evalState, execState, runState, state)
import           Data.Char                            (ord)
import qualified Data.Foldable                        as Fold
import qualified Data.IntervalMap.Interval            as I
import           Data.List                            (elemIndices)
import           Data.Maybe                           (fromMaybe)
-- import           Data.Monoid                          (mempty)
import           Data.Vector.Unboxed                  (Vector, (!))
import qualified Data.Vector.Unboxed                  as DVU
import           Data.Word                            (Word8, Word16)
import           System.IO                            (hPutStrLn, stderr)
import           System.Random                        (Random, StdGen, getStdGen, randomR, setStdGen)
import           Test.Framework                       (Test, defaultMain, plusTestOptions, testGroup)
import           Test.Framework.Options               (TestOptions' (..))
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertBool)
import           Test.QuickCheck                      (Large, NonNegative, Property, choose, forAll, getLarge, getNonNegative)

#if defined(TEST_DEBUG)
import           Debug.Trace
import           Text.Printf
#endif

import qualified Machine.MemorySystem     as M
import           Machine.Tests.TestDevice (mkTestDevice, mkVideoDevice, vidCols, vidLinearSize, vidRows, videoTestPattern)
import           Machine.Utils            (ShowHex (as0xHexS))

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
  do
    stdGen  <- getStdGen
    -- Generate the various random vectors we need to test:
    let ((romImg, writePairs), stdGen') = runState ((,) <$> generateROMImg 4096
                                                        <*> state (generateRandWrites 32768)
                                                   ) stdGen
        options                         = TestParams { randROMImg     = DVU.fromList romImg
                                                     , randWrites     = DVU.fromList writePairs
                                                     , randWritesMVec = writeRAMInitial randWritesSize
                                                     , randWritesMem  = writeRAMMsys randWritesBase randWritesSize
                                                     }
    setStdGen stdGen'
    defaultMain (mkMsysTests options)
  where
    generateROMImg size = state (finiteRandList (0, 0xff) size)

    randWritesBase = 0x0
    randWritesSize = 0x1000 :: Int

    generateRandWrites lim gen =
      let recombine (bytes, (addrs, gen')) = (zip addrs bytes, gen')
      in  recombine $ second (finiteRandList (0, fromIntegral (randWritesSize - 1)) lim) $ finiteRandList (0, 0xff) lim gen

finiteRandList :: (Random a) => (a, a) -> Int -> StdGen -> ([a], StdGen)
finiteRandList range lim = runState (replicateM lim (state (randomR range)))

-- Generated data that gets used by various tests...
data TestParams =
  TestParams
  { randROMImg     :: Vector Word8
  , randWrites     :: Vector (Word16, Word8)
  , randWritesMVec :: Vector Word8
  , randWritesMem  :: TestMemSystem
  }

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The tests...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

mkMsysTests :: TestParams -> [Test]
mkMsysTests args =
  [ testGroup "Random lists"
    [ testCase "Two random lists, l1 /= l2 " test_randLists

    ]
  , testGroup "Memory system construction"
    [ testCase "One ROM region             " (test_mkROMRegion args)
    , testCase "Two ROM regions            " (test_mkROMRegion2 args)
    , testCase "Two RAM regions            " (test_mkRAMRegion3 args)
    , testCase "Monoid 'mempty'            " (test_mkMEmpty args)
    , testCase "Monoid 'mappend'           " (test_mappend args)
    ]
  , testGroup "readROM"
    [ testCase "Read one byte              " (test_ROMread1 args)
    , testCase "Read sequential            " (test_ROMsequential args)
    , plusTestOptions (mkLargeTests (DVU.length readROMImg * 4))
                      (testProperty "Random reads               " prop_ROMrandom)
    , testCase "Random image, one byte     " (test_randROMread1 args)
    , testCase "Random image, sequential   " (test_randROMsequential args)
    , plusTestOptions (mkLargeTests (DVU.length (randROMImg args) * 4))
                      (testProperty "Random image, random reads " (prop_randROMrandom args))
    ]
  , testGroup "ROM with gap"
    [ testCase "Read before gap            " (test_gapROMBefore args)
    , testCase "Read entire ROM            " (test_gapROMTotal args)
    {- , testCase "Sliding window read        " (test_gapWindows args) -}
    ]
  , testGroup "Patch/forced writes"
    [ testCase "patchROM01                 " (test_patchROM01 args)
    , testCase "patchROM02                 " (test_patchROM02 args)
    , testCase "patchROM03                 " (test_patchROM03 args)
    ]
  , testGroup "RAM write"
    [ testCase "Write/read one byte        " (test_RAMwrite1 args)
    , testCase "Write/read five bytes      " (test_RAMwrite5 args)
    , testCase "WriteN/read five bytes     " (test_RAMwrite5n args)
    , testCase "OvewriteN five bytes       " (test_RAMwrite5n2 args)
    , testCase "Sequential write           " (test_RAMSequentialWrite args)
    {- , plusTestOptions (mkLargeTests (DVU.length (randWrites args) `div` 8))
                      (testProperty "Random write pairs         " (test_RAMRandReads args 0 0x1000)) -}
    ]
  , testGroup "Memory-mapped devices"
    [ testCase "Create TestDevice          " (test_MemMappedDeviceCreate args)
    , testCase "Repeated TestDevice reads  " (test_MemMappedDeviceReads args)
    , testCase "Create VideoDevice         " (test_VideoDeviceCreate args)
    , testCase "Read from VideoDevice      " (test_VideoDeviceReads args)
    , testCase "Write to VideoDevice       " (test_VideoDeviceWrite args)
    , testCase "Mixed device types         " (test_MixedDevices args)
    ]
  ]
  where
    -- 'mempty' is really the TestOptions record instantiated with all of the
    -- members set to Nothing.
    mkLargeTests nTests = mempty { topt_maximum_generated_tests = Just nTests
                                 , topt_maximum_unsuitable_generated_tests = Just (nTests * 5)
                                 }

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
  = Just (lName ++ " length (" ++ show (DVU.length l) ++ ") less than " ++ rName ++ " (" ++ show (DVU.length r) ++ ")")
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
-- Utility functions
--
-- For non-device reads, we're not interested in the altered memory system, because the memory system doesn't change.
-- mRead_ and mReadN_ discard the memory system part of the returned pair. You'd think that eta reduction would work
-- here too, but it doesn't.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

mRead_ :: ( Integral addrType
          , Integral wordType
          , DVU.Unbox wordType
          )
       => addrType
       -> M.MemorySystem addrType wordType
       -> wordType
mRead_ addr = fst . M.mRead addr
{-# INLINEABLE mRead_ #-}

mReadN_ :: ( Integral addrType
           , Integral wordType
           , Show wordType
           , DVU.Unbox wordType
#if defined(TEST_DEBUG)
           , PrintfArg addrType
           , Show addrType
           , PrintfArg wordType
#endif
           )
        => addrType
        -- ^ Starting address
        -> Int
        -- ^ Number of words to read
        -> M.MemorySystem addrType wordType
        -- ^ The memory system from which to read
        -> Vector wordType
        -- ^ Contents read from memory
mReadN_ addr nWords = fst . M.mReadN addr nWords
{-# INLINEABLE mReadN_ #-}


-- | The test system
type TestMemSystem = M.MemorySystem Word16 Word8

-- | Test system constructor
testSystem :: TestMemSystem
testSystem = M.initialMemorySystem

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- The tests...
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

test_randLists :: Assertion
test_randLists =
  getStdGen >>= (\stdGen -> let (l1, gen1) = finiteRandList (0, 0x1000) 10240 stdGen :: ([Int], StdGen)
                                (l2, gen2) = finiteRandList (0, 0x1000) 10240 gen1   :: ([Int], StdGen)
                            in  setStdGen gen2 >> assertBool "l1 == l2" (l1 /= l2))

test_mkROMRegion :: TestParams -> Assertion
test_mkROMRegion _args =
  let img      = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys     = M.mkROMRegion 0 img testSystem
      rlist    = map fst (M.regionList msys)
      nRegions = M.countRegions msys == 2
      mRegions = rlist == [I.ClosedInterval 0 4095, I.ClosedInterval 4096 65535]
  in  assertBool (if   not nRegions
                  then "Expected 2 regions, got " ++ show (M.countRegions msys)
                  else "Expected 4096 Word8 region, got " ++ show rlist)
                 (nRegions && mRegions)

test_mkROMRegion2 :: TestParams -> Assertion
test_mkROMRegion2 _args =
  let img      = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys     = M.mkROMRegion 4096 img (M.mkROMRegion 0 img testSystem)
      rlist    = map fst (M.regionList msys)
      nRegions = M.countRegions msys == 3
      mRegions = rlist == [I.ClosedInterval 0 4095, I.ClosedInterval 4096 8191, I.ClosedInterval 8192 65535]
  in  assertBool (if   not nRegions
                  then "Expected 3 regions, got " ++ show (M.countRegions msys)
                  else "Expected 3 intervals, got " ++ show rlist)
                 (nRegions && mRegions)

test_mkRAMRegion3 :: TestParams -> Assertion
test_mkRAMRegion3 _args =
  let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 testSystem)
      rlist = map fst (M.regionList msys)
  in  assertBool "Expected 4 RAM regions"
                 (M.countRegions msys == 4 && rlist == [ I.ClosedInterval 0x0000 0x0fff
                                                       , I.ClosedInterval 0x1000 0x13ff
                                                       , I.ClosedInterval 0x1400 0x23ff
                                                       , I.ClosedInterval 0x2400 0xffff
                                                       ])

test_mkMEmpty :: TestParams -> Assertion
test_mkMEmpty _args =
  let msys = mempty :: M.MemorySystem Word16 Word8
  in  assertBool "Expecitng an empty MemorySystem"
                 (M.countRegions msys == 1)

test_mappend :: TestParams -> Assertion
test_mappend _args =
  let imgA     = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msysA    = M.mkROMRegion 0 imgA testSystem
      msysB    = M.mkRAMRegion 0x2000 0x1000 <> M.mkRAMRegion 0x1000 0x1000 $ testSystem
      msys     = msysA <> msysB
      rlist    = map fst (M.regionList msys)
  in  assertBool (if   M.countRegions msys /= 4
                  then "Expected 4 regions: " ++ show (M.countRegions msys)
                  else "Interval list doesn't match: " ++ show rlist)
                 (M.countRegions msys == 4 && rlist == [ I.ClosedInterval 0x0000 0x0fff
                                                       , I.ClosedInterval 0x1000 0x1fff
                                                       , I.ClosedInterval 0x2000 0x2fff
                                                       , I.ClosedInterval 0x3000 0xffff
                                                       ])

-- | Simple ROM
simpleROMMsys  :: TestMemSystem
simpleROMMsys = M.mkROMRegion 0 readROMImg testSystem

readROMImg :: Vector Word8
readROMImg  = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256))

-- | Random value ROM
randROMMsys :: Vector Word8 -> TestMemSystem
randROMMsys img = M.mkROMRegion 0 img testSystem

test_ROMread1 :: TestParams -> Assertion
test_ROMread1 _args = assertBool "Read byte from ROM, 1 != 1" (mRead_ 1 simpleROMMsys == 1)

test_ROMsequential :: TestParams -> Assertion
test_ROMsequential _args =
  let mReads = [mRead_ (fromIntegral i) simpleROMMsys == fromIntegral (i `mod` 256) | i <- [0..DVU.length readROMImg - 1]]
      wrong  = elemIndices False mReads
  in  assertBool ("Mismatched reads at indices: " ++ show wrong) (and mReads)

prop_ROMrandom :: Property
prop_ROMrandom =
  let readROM idx = mRead_ (fromIntegral idx) simpleROMMsys == fromIntegral (idx  `mod` 256)
  in  forAll (choose (0, DVU.length readROMImg - 1)) readROM

test_randROMread1 :: TestParams -> Assertion
test_randROMread1 args =
  let img = randROMImg args
      val = mRead_ 1 (randROMMsys img)
  in  assertBool ("Read byte 1 from random ROM image: expected 1, got " ++ show val)
                 (val == img ! 1)

test_randROMsequential :: TestParams -> Assertion
test_randROMsequential args =
  let img    = randROMImg args
      sys    = randROMMsys img
      mReads = [mRead_ (fromIntegral i) sys == img ! i | i <- [0..DVU.length readROMImg - 1]]
      wrong  = elemIndices False mReads
  in  assertBool ("Mismatched reads at indices: " ++ show wrong) (and mReads)

prop_randROMrandom :: TestParams -> Property
prop_randROMrandom args =
  let img         = randROMImg args
  in  forAll (choose (0, DVU.length img - 1)) (\idx -> mRead_ (fromIntegral idx) (randROMMsys img) == img ! idx)

-- | start address for the gapped ROM region tests. this is a prime number instead of a power-of-2.
gapROM_addr_1, gapROM_addr_2 :: Word16
gapROM_addr_1 = 0xa9f {- 2719 -}
gapROM_addr_2 = 0xf43 {- 3907 -}

gapROM_len_1, gapROM_len_2 :: Int
gapROM_len_1  = 0x469 {- 1129 -}
gapROM_len_2  = 0xfbb {- 4027 -}

gapROMGap, gapROMTotal :: Int
gapROMGap     = fromIntegral (gapROM_addr_2 - gapROM_addr_1) - gapROM_len_1
gapROMTotal   = fromIntegral (gapROM_len_1 + gapROM_len_2) + gapROMGap

gapROMImg_1, gapROMImg_2 :: Vector Word8
gapROMImg_1   = DVU.generate gapROM_len_1 (\x -> fromIntegral (x `mod` 256))
gapROMImg_2   = DVU.generate gapROM_len_2 (\x -> fromIntegral (x `mod` 256))

gapROMMsys :: TestMemSystem
gapROMMsys    = M.mkROMRegion gapROM_addr_2 gapROMImg_2 $ M.mkROMRegion gapROM_addr_1 gapROMImg_1 testSystem

test_gapROMTotal :: TestParams -> Assertion
test_gapROMTotal _args =
  do
    let (memvec, _) = M.mReadN gapROM_addr_1 gapROMTotal gapROMMsys
        cmpvec      = DVU.concat [gapROMImg_1, DVU.replicate gapROMGap 0, gapROMImg_2]
    -- printf "cmpvec %s\nmemvec %s\n" (show cmpvec) (show memvec)
    assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
               (memvec == cmpvec)

test_gapROMBefore :: TestParams -> Assertion
test_gapROMBefore _args =
  do
    let (memvec, _) = M.mReadN (gapROM_addr_1 - 17) (17 + 19) gapROMMsys
    let cmpvec      = DVU.concat [DVU.replicate 17 0, DVU.slice 0 19 gapROMImg_1]
    -- printf "cmpvec %s\nmemvec %s\n" (show cmpvec) (show memvec)
    assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
               (memvec == cmpvec)

test_gapWindows :: TestParams -> Assertion
test_gapWindows _args =
  let totalGapVec = DVU.concat [ gapROMImg_1
                               , DVU.replicate (fromIntegral (gapROM_addr_2 - gapROM_addr_1) - gapROM_len_1) 0
                               , gapROMImg_2
                               ]
      totalLen    = DVU.length totalGapVec

      inner toRead = and <$> sequence [gapReadOverWindow toRead (fromIntegral i) | i <- [0..(totalLen - toRead)]]

      gapReadOverWindow toRead offs =
        let memvec = mReadN_ (gapROM_addr_1 + offs) toRead gapROMMsys
            cmpvec = DVU.slice (fromIntegral offs) toRead totalGapVec
            passed = memvec == cmpvec
        in  unless passed
              ( mapM_ (hPutStrLn stderr)
                  [ "gapWindows/inner@" ++ as0xHexS offs ++
                    ", toRead = " ++ show toRead ++
                    " cmpvec " ++ show (DVU.length cmpvec) ++
                    " memvec " ++ show (DVU.length memvec)
                  , as0xHexS (DVU.toList memvec)
                  , as0xHexS (DVU.toList cmpvec)
                  ]
              )
            >> return passed

  in  sequence [inner (totalLen - w) | w <- [0..fromIntegral(totalLen - 1)]]
      >>= (assertBool "read gapped window failed" . and)

test_patchROM01 :: TestParams -> Assertion
test_patchROM01 _args =
  let img    = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      sys    = M.mkROMRegion 0 img testSystem
      pvec   = DVU.fromList [ 0x11, 0x22, 0x33, 0x44, 0x55, 0x66]
      sys'   = M.mPatch 14 pvec sys
      cmpvec = DVU.concat [ DVU.take 2 (DVU.drop 12 img), pvec, DVU.take 2 (DVU.drop 20 img)]
      memvec = mReadN_ 12 (DVU.length cmpvec) sys'
  in  -- printf "cmpvec %s\nmemvec %s\n" (show cmpvec) (show memvec) >>
      assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec)

patchROMMsys_1 :: TestMemSystem
patchROMMsys_1 = M.mkRAMRegion 20 4 (M.mkRAMRegion 0 16 testSystem)

test_patchROM02 :: TestParams -> Assertion
test_patchROM02 _args =
  let pvec   = DVU.concat [ DVU.fromList [0x11, 0x22 ], DVU.replicate 4 0, DVU.fromList [ 0x33, 0x44, 0x55, 0x66 ]]
      msys'  = M.mPatch 14 pvec patchROMMsys_1
      cmpvec = DVU.concat [ DVU.fromList [ 0x00, 0x00 ], pvec ]
      memvec = mReadN_ 12 (DVU.length cmpvec) msys'
  in assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                (memvec == cmpvec)

test_patchROM03 :: TestParams -> Assertion
test_patchROM03 _args =
  let pvec   = DVU.concat [ DVU.fromList [ 0x11, 0x22 ], DVU.replicate 4 0, DVU.fromList [ 0x33, 0x44, 0x55, 0x66 ]]
      sys'   = M.mPatch 14 pvec patchROMMsys_1
      cmpvec = DVU.concat [ DVU.fromList [ 0x00, 0x00 ], pvec, DVU.fromList [ 0x00, 0x00, 0x00 ]]
      memvec = mReadN_ 12 (DVU.length cmpvec) sys'
  in assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                (memvec == cmpvec)

writeRAMInitial :: Int -> Vector Word8
writeRAMInitial ramSize = DVU.generate ramSize (\x -> fromIntegral (x `mod` 256))
{-# INLINABLE writeRAMInitial #-}

writeRAMMsys :: Word16 -> Int -> TestMemSystem
writeRAMMsys ramBase ramSize = M.mPatch ramBase initial sys
  where
    initial = writeRAMInitial ramSize
    sys     = M.mkRAMRegion ramBase ramSize testSystem
{-# INLINABLE writeRAMMsys #-}

test_RAMwrite1 :: TestParams -> Assertion
test_RAMwrite1 _args =
  let ramSize = 12 * 1024
      ramBase = 0x0000
      msys    = M.mWrite 0 0xff (writeRAMMsys ramBase ramSize)
      mem0    = mRead_ 0 msys
  in  assertBool ("Write RAM, expected 0xff, got " ++ as0xHexS mem0)
                 (mem0 == 0xff && M.sanityCheck msys)

test_RAMwrite5 :: TestParams -> Assertion
test_RAMwrite5 _args =
  let initRAM = writeRAMMsys 0 0x00ff
      msys    = Fold.foldr writeRAM initRAM [(0, 0xff), (3, 0xaa), (2, 0xbb), (1, 0xcc), (4, 0x55)]
      memvec  = mReadN_ 0 7 msys
      cmpvec  = DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55, 0x05, 0x06]
  in  assertBool (if not (M.sanityCheck msys)
                  then "msys sanity check failed: " ++ show msys
                  else fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec && M.sanityCheck msys)

test_RAMwrite5n :: TestParams -> Assertion
test_RAMwrite5n _args =
  let msys   = M.mWriteN 0x11 (DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55]) (writeRAMMsys 0 0x00ff)
      memvec = mReadN_ 0x11 7 msys
      cmpvec = DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55, 0x016, 0x17]
  in  assertBool (if not (M.sanityCheck msys)
                  then "msys sanity check failed: " ++ show msys
                  else fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec && M.sanityCheck msys)

test_RAMwrite5n2 :: TestParams -> Assertion
test_RAMwrite5n2 _args =
  let msys   = M.mWriteN 0x11 (DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55]) (writeRAMMsys 0 0x00ff)
      msys'   = M.mWriteN 0x13 (DVU.fromList [0xbe, 0xee, 0x66]) msys
      memvec = mReadN_ 0x11 7 msys'
      cmpvec = DVU.fromList [0xff, 0xcc, 0xbe, 0xee, 0x66, 0x016, 0x17]
  in  assertBool (if not (M.sanityCheck msys)
                  then "msys sanity check failed: " ++ show msys
                  else fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec && M.sanityCheck msys)

test_RAMSequentialWrite :: TestParams -> Assertion
test_RAMSequentialWrite _args =
  let ramSize = 0x1000
      ramBase = 0x0100 :: Word16
      pairs   = [(fromIntegral addr + ramBase, val) | addr <- [0..ramSize - 1], let val = fromIntegral (addr `mod` 256)]
      msys    = Fold.foldr writeRAM (M.mkRAMRegion ramBase ramSize testSystem) pairs
      memvec  = mReadN_ ramBase ramSize msys
      cmpvec  = DVU.generate ramSize (\x -> fromIntegral (x `mod` 256))
  in assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                (memvec == cmpvec && M.sanityCheck msys)

test_RAMRandReads :: TestParams -> Word16 -> Int -> Property
test_RAMRandReads args ramBase ramSize = forAll (choose (1, DVU.length addrPairs `div` 2)) testWrites
  where
    addrPairs    = randWrites args
    ramMsys      = randWritesMem args
    ramMvec      = randWritesMVec args
    testWrites :: Int -> NonNegative (Large Int) -> Bool
    testWrites n m =
      let m'      = getLarge (getNonNegative m)
          writes  = DVU.slice n (min m' (DVU.length addrPairs - n)) addrPairs
          msys    = DVU.foldr' writeRAM ramMsys writes
          memvec  = mReadN_ ramBase ramSize msys
          cmpvec  = DVU.update ramMvec (DVU.map (first fromIntegral) writes)
      in  M.sanityCheck msys && memvec == cmpvec

writeRAM :: (Word16, Word8)
         -> TestMemSystem
         -> TestMemSystem
writeRAM (addr, val) {-sys-} = M.mWrite addr val {-sys-}
{-# INLINABLE writeRAM #-}

testMemMappedDev :: TestMemSystem
testMemMappedDev = M.mkDevRegion 0x100 0x1 mkTestDevice testSystem

test_MemMappedDeviceCreate :: TestParams -> Assertion
test_MemMappedDeviceCreate _args =
  let msys         = testMemMappedDev
      rlist        = map fst (M.regionList msys)
      nRegions     = M.countRegions msys == 3
      mRegions     = rlist == [ I.ClosedInterval 0x0000 0x00ff
                              , I.ClosedInterval 0x0100 0x0100
                              , I.ClosedInterval 0x0101 0xffff
                              ]
      diagnose | not nRegions
               = "Expected 3 memory regions" ++ show (M.countRegions msys)
               | not mRegions
               = "Region mismatch: " ++ show rlist
               | otherwise
               = "Huh?"
  in  assertBool diagnose (nRegions && mRegions)

test_MemMappedDeviceReads :: TestParams -> Assertion
test_MemMappedDeviceReads _args =
  let testDev       = testMemMappedDev
      nElts         = 18
      readMem       = M.mRead 0x100
      mReads        = evalState (replicateM nElts (state readMem)) testDev
      expected      = take nElts (iterate (+ 1) 19)
  in  assertBool ("Expected: " ++ show expected ++ ", got " ++ show mReads) (mReads == expected)

test_VideoDeviceCreate :: TestParams -> Assertion
test_VideoDeviceCreate _args =
  let vidsys       = mkVideoDevice 0x3c00 testSystem
      rlist        = map fst (M.regionList vidsys)
      nRegions     = M.countRegions vidsys == 3
      mRegions     = rlist == [ I.ClosedInterval 0x0000 0x3bff
                              , I.ClosedInterval 0x3c00 0x41ff
                              , I.ClosedInterval 0x4200 0xffff
                              ]
      diagnose | not nRegions
               = "Expected 3 memory regions" ++ show (M.countRegions vidsys)
               | not mRegions
               = "Region mismatch: " ++ show rlist
               | otherwise
               = "Huh?"
  in  assertBool diagnose (nRegions && mRegions)

test_VideoDeviceReads :: TestParams -> Assertion
test_VideoDeviceReads _args =
  let vidsys         = mkVideoDevice 0x3c00 testSystem
      checkPattern  = map (fromIntegral . ord) $ take vidLinearSize (cycle (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']))
      vram          = mReadN_ 0x3c00 vidLinearSize vidsys
  in  assertBool "test patterns did not match." (vram == DVU.fromList checkPattern)

test_VideoDeviceWrite :: TestParams -> Assertion
test_VideoDeviceWrite _args =
    let vidsys         = mkVideoDevice 0x3c00 testSystem
        rowPattern c   = [ fromIntegral . ord $ if c /= c' then ' ' else '#' | c' <- [0..vidCols - 1]]
        rowOffset r    = r * vidCols
        vidOffset r c  = rowOffset r + c
        rowAddress r   = fromIntegral (0x3c00 + rowOffset r)
        vidAddress r c = fromIntegral (0x3c00 + vidOffset r c)
        -- Write to individual rows: exercises M.mWriteN
        doRowTest r    = let memvec    = rowTest r
                             cmpvec    = cmpRow r
                             cmpResult = compareVectors memvec cmpvec "doRowTest memvec" "cmpvec"
                         in  assertBool (fromMaybe "successful" cmpResult) (memvec == cmpvec)
        rowTest r      = mReadN_ 0x3c00 vidLinearSize (M.mWriteN (rowAddress r) (DVU.fromList (rowPattern r)) vidsys)
        cmpRow  r      = DVU.fromList (take (rowOffset r) videoTestPattern
                                       ++ rowPattern r
                                       ++ drop (rowOffset (r + 1)) videoTestPattern)
        -- Write to individual columns: exercises m.mWrite
        doColTest c    = let memvec = colTest c
                             cmpvec = cmpCol c
                         in  assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                                        (memvec == cmpvec)
        -- Yes, writeCol looks weird, but we're using StateT to propagate the altered memory system forward
        -- while writing to successive columns. Need to return some value, in this case, (r, c, ch) unmolested,
        -- in addition to the updated memory system.
        writeCol (r, c, ch) sys'  = ((), M.mWrite (vidAddress r c) ch sys')
        colTest c                 = mReadN_ 0x3c00 vidLinearSize (execState (traverse (state . writeCol) (colPattern c)) vidsys)
        cmpCol c                  = DVU.update (DVU.fromList videoTestPattern) (cmpOffsets c)
        cmpOffsets c              = DVU.fromList (map (\(r', c', ch) -> (vidOffset r' c', ch)) (colPattern c))
        colPattern c              = [ (r, c, fromIntegral . ord $ if c /= r then ' ' else '#') | r <- [0..vidRows - 1]]
    in  sequence_ [ doRowTest r | r <- [0..vidRows - 1]] >> sequence_ [ doColTest c | c <- [0..vidCols - 1]]


test_MixedDevices :: TestParams -> Assertion
test_MixedDevices _args =
  let msys            = mkVideoDevice 0x3c00 testMemMappedDev
      rlist           = map fst (M.regionList msys)
      nRegions        = M.countRegions msys == 5
      expectedRegions = [ I.ClosedInterval 0x0000 0x00ff
                        , I.ClosedInterval 0x0100 0x0100
                        , I.ClosedInterval 0x0101 0x3bff
                        , I.ClosedInterval 0x3c00 (0x3c00 + fromIntegral (vidLinearSize - 1))
                        , I.ClosedInterval (0x3c00 + fromIntegral vidLinearSize) 0xffff
                        ]
      mRegions        = rlist == expectedRegions
      diagnose | not nRegions
               = "Expected 5 memory regions: " ++ show (M.countRegions msys)
               | not mRegions
               = "Region mismatch: " ++ show rlist ++ " expected " ++ show expectedRegions
               | otherwise
               = "Huh?"
  in  assertBool diagnose (nRegions && mRegions)
