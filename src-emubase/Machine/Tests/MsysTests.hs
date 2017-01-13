
{- | 'MemorySystem' exercise module -}

module Main where

import           Control.Arrow                        (first)
import           Control.Monad                        (replicateM, unless)
import           Control.Monad.State.Strict           (evalState, get, put, runState)
import qualified Data.Foldable                        as Fold (foldl)
import qualified Data.IntervalMap.Interval            as I
import           Data.List                            (elemIndices)
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          (mempty)
import           Data.Vector.Unboxed                  (Vector, (!))
import qualified Data.Vector.Unboxed                  as DVU
import           Data.Word
import           System.IO                            (hPutStrLn, stderr)
import           System.Random                        (Random, StdGen, getStdGen, randomR, setStdGen)
import           Test.Framework                       (Test, defaultMain, plusTestOptions, testGroup)
import           Test.Framework.Options               (TestOptions' (..))
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertBool)
import           Test.QuickCheck                      (Large, NonNegative, Property, choose, forAll, getLarge, getNonNegative)

-- import           Debug.Trace

import qualified Machine.MemorySystem                 as M
import           Machine.Tests.TestDevice             (mkTestDevice)
import           Machine.Utils                        (ShowHex, as0xHexS)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
  do
    stdGen  <- getStdGen
    -- Generate the various random vectors we need to test:
    let (romImg, stdGen')        = generateROMImg stdGen 4096
        (writePairs, stdGen'')   = generateRandWrites stdGen' 32768
        options                  = TestParams { randROMImg     = DVU.fromList romImg
                                              , randWrites     = DVU.fromList writePairs
                                              , randWritesMVec = writeRAMInitial randWritesSize
                                              , randWritesMem  = writeRAMMsys randWritesBase randWritesSize
                                              }
    setStdGen stdGen''
    defaultMain (mkMsysTests options)
  where
    generateROMImg gen = finiteRandList gen (0, 0xff)

    randWritesBase = 0x0
    randWritesSize = 0x1000 :: Int

    generateRandWrites gen lim =
      let (bytes, gen')  = finiteRandList gen  (0, 0xff) lim
          (addrs, gen'') = finiteRandList gen' (0, fromIntegral (randWritesSize - 1)) lim
      in  (zip addrs bytes, gen'')

finiteRandList :: (Random a) => StdGen -> (a, a) -> Int -> ([a], StdGen)
finiteRandList gen range lim =
  let -- There is eta reduction here: the State 's' is an implied extra argument to this function. Note that 'return x'
      -- is really '(return x) s'
      -- genRandom :: StateT StdGen Identity a
      genRandom =
        get >>= (\gen' -> let (x, gen'') = randomR range gen'
                          in  put gen'' >> return x)
  in  runState (replicateM lim genRandom) gen

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
    -- , testCase "Sliding window read        " (test_gapWindows args)
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
    {-, plusTestOptions (mkLargeTests (DVU.length (randWrites args) `div` 8))
                      (testProperty "Random write pairs         " (test_RAMRandReads args 0 0x1000))-}
    ]
  , testGroup "Memory-mapped devices"
    [ testCase "Create                     " (test_MemMappedDeviceCreate args)
    , testCase "Multiple reads             " (test_MemMappedDeviceReads args)
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
-- Utility functions
--
-- For non-device reads, we're not interested in the altered memory system, because the memory system doesn't change.
-- mRead_ and mReadN_ discard the memory system part of the returned pair. You'd think that eta reduction would work
-- here too, but it doesn't.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

mRead_ :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
          M.MemorySystem addrType wordType
       -> addrType
       -> wordType
mRead_ msys = fst . M.mRead msys
{-# INLINEABLE mRead_ #-}

mReadN_ :: (Integral addrType, Num wordType, ShowHex addrType, DVU.Unbox wordType) =>
           M.MemorySystem addrType wordType
        -- ^ The memory system from which to read
        -> addrType
        -- ^ Starting address
        -> Int
        -- ^ Number of words to read
        -> Vector wordType
        -- ^ Contents read from memory
mReadN_ msys addr = fst . M.mReadN msys addr
{-# INLINEABLE mReadN_ #-}

-- | The test memory system
type TestMemSystem = M.MemorySystem Word16 Word8

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- The tests...
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

test_randLists :: Assertion
test_randLists =
  getStdGen >>= (\stdGen -> let (l1, gen1) = finiteRandList stdGen (0, 0x1000) 10240 :: ([Int], StdGen)
                                (l2, gen2) = finiteRandList gen1   (0, 0x1000) 10240 :: ([Int], StdGen)
                            in  setStdGen gen2 >> assertBool "l1 == l2" (l1 /= l2))

test_mkROMRegion :: TestParams -> Assertion
test_mkROMRegion _args =
  let img      = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys     = M.mkROMRegion 0 img M.initialMemorySystem :: TestMemSystem
      rlist    = map fst (M.regionList msys)
      nRegions = M.countRegions msys == 1
      mRegions = rlist == [I.IntervalCO 0 4096]
  in  assertBool (if   not nRegions
                  then "Expected 1 region, got " ++ show (M.countRegions msys)
                  else "Expected 4096 Word8 region, got " ++ show rlist)
                 (nRegions && mRegions)

test_mkROMRegion2 :: TestParams -> Assertion
test_mkROMRegion2 _args =
  let img   = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys  = M.mkROMRegion 4096 img (M.mkROMRegion 0 img M.initialMemorySystem :: TestMemSystem)
      rlist = map fst (M.regionList msys)
      nRegions = M.countRegions msys == 2
      mRegions = rlist == [I.IntervalCO 0 4096, I.IntervalCO 4096 8192]
  in  assertBool (if   not nRegions
                  then "Expected 2 regions, got " ++ show (M.countRegions msys)
                  else "Expected 2 intervals, got " ++ show rlist)
                 (nRegions && mRegions)

test_mkRAMRegion3 :: TestParams -> Assertion
test_mkRAMRegion3 _args =
  let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 M.initialMemorySystem :: TestMemSystem)
      rlist = map fst (M.regionList msys)
  in  assertBool "Expected 2 RAM regions"
                 (M.countRegions msys == 2 && rlist == [I.IntervalCO 0 0x1000, I.IntervalCO 0x1400 0x2400])

test_mkMEmpty :: TestParams -> Assertion
test_mkMEmpty _args =
  let msys = mempty :: TestMemSystem
  in  assertBool "Expecitng an empty MemorySystem"
                 (M.countRegions msys == 0 && null (M.regionList msys))

test_mappend :: TestParams -> Assertion
test_mappend _args =
  let imgA     = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msysA    = M.mkROMRegion 0 imgA M.initialMemorySystem :: TestMemSystem
      msysB    = M.mkRAMRegion 0x2000 0x1000 (M.mkRAMRegion 0x1000 0x1000
                               M.initialMemorySystem :: TestMemSystem)
      msys     = msysA `mappend` msysB
      rlist    = map fst (M.regionList msys)
  in  assertBool (if   M.countRegions msys /= 3
                  then "Expected 3 regions: " ++ show (M.countRegions msys)
                  else "Interval list doesn't match: " ++ show rlist)
                 (M.countRegions msys == 3 && rlist == [ I.IntervalCO 0x0000 0x1000
                                                       , I.IntervalCO 0x1000 0x2000
                                                       , I.IntervalCO 0x2000 0x3000
                                                       ])

-- | Simple ROM
simpleROMMsys  :: TestMemSystem
simpleROMMsys = M.mkROMRegion 0 readROMImg M.initialMemorySystem

readROMImg :: Vector Word8
readROMImg  = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256))

-- | Random value ROM
randROMMsys :: Vector Word8 -> TestMemSystem
randROMMsys img = M.mkROMRegion 0 img M.initialMemorySystem

test_ROMread1 :: TestParams -> Assertion
test_ROMread1 _args = assertBool "Read byte from ROM, 1 != 1" (mRead_ simpleROMMsys 1 == 1)

test_ROMsequential :: TestParams -> Assertion
test_ROMsequential _args =
  let mReads = [mRead_ simpleROMMsys (fromIntegral i) == fromIntegral (i `mod` 256) | i <- [0..DVU.length readROMImg - 1]]
      wrong  = elemIndices False mReads
  in  assertBool ("Mismatched reads at indices: " ++ show wrong) (and mReads)

prop_ROMrandom :: Property
prop_ROMrandom =
  let readROM idx = mRead_ simpleROMMsys (fromIntegral idx) == fromIntegral (idx  `mod` 256)
  in  forAll (choose (0, DVU.length readROMImg - 1)) readROM

test_randROMread1 :: TestParams -> Assertion
test_randROMread1 args =
  let img       = randROMImg args
      msys      = randROMMsys img
      val       = mRead_ msys 1
  in assertBool ("Read byte 1 from random ROM image: expected 1, got " ++ show val)
                (val == img ! 1)

test_randROMsequential :: TestParams -> Assertion
test_randROMsequential args =
  let img    = randROMImg args
      msys   = randROMMsys img
      mReads = [mRead_ msys (fromIntegral i) == img ! i | i <- [0..DVU.length readROMImg - 1]]
      wrong  = elemIndices False mReads
  in  assertBool ("Mismatched reads at indices: " ++ show wrong) (and mReads)

prop_randROMrandom :: TestParams -> Property
prop_randROMrandom args =
  let img         = randROMImg args
  in  forAll (choose (0, DVU.length img - 1)) (\idx -> mRead_ (randROMMsys img) (fromIntegral idx) == img ! idx)

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

gapROMMsys :: TestMemSystem
gapROMMsys    = M.mkROMRegion gapROM_addr_2 gapROMImg_2 (M.mkROMRegion gapROM_addr_1 gapROMImg_1 M.initialMemorySystem)

test_gapROMTotal :: TestParams -> Assertion
test_gapROMTotal _args =
  do
    let (memvec, _) = M.mReadN gapROMMsys gapROM_addr_1 gapROMTotal
        cmpvec      = DVU.concat [gapROMImg_1, DVU.replicate gapROMGap 0, gapROMImg_2]
    assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
               (memvec == cmpvec)

test_gapROMBefore :: TestParams -> Assertion
test_gapROMBefore _args =
  do
    let (memvec, _) = M.mReadN gapROMMsys (gapROM_addr_1 - 17) (17 + 19)
    let cmpvec      = DVU.concat [DVU.replicate 17 0, DVU.slice 0 19 gapROMImg_1]
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
        let memvec = mReadN_ gapROMMsys (gapROM_addr_1 + offs) toRead
            cmpvec = DVU.slice (fromIntegral offs) toRead totalGapVec
            passed = memvec == cmpvec
        in  unless passed
              ( hPutStrLn stderr ("gapWindows/inner@" ++ as0xHexS offs ++
                                  ", toRead = " ++ show toRead ++
                                  " cmpvec " ++ show (DVU.length cmpvec) ++
                                  " memvec " ++ show (DVU.length memvec))
                >> hPutStrLn stderr (as0xHexS (DVU.toList memvec))
                >> hPutStrLn stderr (as0xHexS (DVU.toList cmpvec))
              )
            >> return passed

  in  and <$> sequence [inner (totalLen - w) | w <- [0..fromIntegral(totalLen - 1)]]
      >>= assertBool "read gapped window failed"

test_patchROM01 :: TestParams -> Assertion
test_patchROM01 _args =
  let img    = DVU.generate 4096 (\x -> fromIntegral (x `mod` 256)) :: Vector Word8
      msys   = M.mkROMRegion 0 img M.initialMemorySystem :: TestMemSystem
      pvec   = DVU.fromList [ 0x11, 0x22, 0x33, 0x44, 0x55, 0x66]
      msys'  = M.mPatch 14 pvec msys
      cmpvec = DVU.concat [ DVU.fromList [0x0c, 0x0d ], pvec, DVU.fromList [ 0x14, 0x15 ]]
      memvec = mReadN_ msys' 12 (DVU.length cmpvec)
  in  assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec)

patchROMMsys_1 :: TestMemSystem
patchROMMsys_1 = M.mkRAMRegion 20 4 (M.mkRAMRegion 0 16 M.initialMemorySystem)

test_patchROM02 :: TestParams -> Assertion
test_patchROM02 _args =
  let pvec   = DVU.concat [ DVU.fromList [0x11, 0x22 ], DVU.replicate 4 0, DVU.fromList [ 0x33, 0x44, 0x55, 0x66 ]]
      msys'  = M.mPatch 14 pvec patchROMMsys_1
      cmpvec = DVU.concat [ DVU.fromList [ 0x00, 0x00 ], pvec ]
      memvec = mReadN_ msys' 12 (DVU.length cmpvec)
  in assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                (memvec == cmpvec)

test_patchROM03 :: TestParams -> Assertion
test_patchROM03 _args =
  let pvec   = DVU.concat [ DVU.fromList [ 0x11, 0x22 ], DVU.replicate 4 0, DVU.fromList [ 0x33, 0x44, 0x55, 0x66 ]]
      msys'  = M.mPatch 14 pvec patchROMMsys_1
      cmpvec = DVU.concat [ DVU.fromList [ 0x00, 0x00 ], pvec, DVU.fromList [ 0x00, 0x00, 0x00 ]]
      memvec = mReadN_ msys' 12 (DVU.length cmpvec)
  in assertBool (fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                (memvec == cmpvec)

writeRAMInitial :: Int -> Vector Word8
writeRAMInitial ramSize = DVU.generate ramSize (\x -> fromIntegral (x `mod` 256))
{-# INLINABLE writeRAMInitial #-}

writeRAMMsys :: Word16 -> Int -> TestMemSystem
writeRAMMsys ramBase ramSize = M.mPatch ramBase initial msys
  where
    initial = writeRAMInitial ramSize
    msys    = M.mkRAMRegion ramBase ramSize M.initialMemorySystem
{-# INLINABLE writeRAMMsys #-}

test_RAMwrite1 :: TestParams -> Assertion
test_RAMwrite1 _args =
  let ramSize = 12 * 1024
      ramBase = 0x0000
      msys    = M.mWrite 0 0xff (writeRAMMsys ramBase ramSize)
      mem0    = mRead_ msys 0
  in  assertBool ("Write RAM, expected 0xff, got " ++ as0xHexS mem0)
                 (mem0 == 0xff && M.sanityCheck msys)

test_RAMwrite5 :: TestParams -> Assertion
test_RAMwrite5 _args =
  let msys    = Fold.foldl writeRAM (writeRAMMsys 0 0x00ff) [(0, 0xff), (3, 0xaa), (2, 0xbb), (1, 0xcc), (4, 0x55)]
      memvec  = mReadN_ msys 0 7
      cmpvec  = DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55, 0x05, 0x06]
  in  assertBool (if not (M.sanityCheck msys)
                  then "msys sanity check failed: " ++ show msys
                  else fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec && M.sanityCheck msys)

test_RAMwrite5n :: TestParams -> Assertion
test_RAMwrite5n _args =
  let msys   = M.mWriteN 0x11 (DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55]) (writeRAMMsys 0 0x00ff)
      memvec = mReadN_ msys 0x11 7
      cmpvec = DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55, 0x016, 0x17]
  in  assertBool (if not (M.sanityCheck msys)
                  then "msys sanity check failed: " ++ show msys
                  else fromMaybe "successful" (compareVectors memvec cmpvec "memvec" "cmpvec"))
                 (memvec == cmpvec && M.sanityCheck msys)

test_RAMwrite5n2 :: TestParams -> Assertion
test_RAMwrite5n2 _args =
  let msys   = M.mWriteN 0x11 (DVU.fromList [0xff, 0xcc, 0xbb, 0xaa, 0x55]) (writeRAMMsys 0 0x00ff)
      msys'  = M.mWriteN 0x13 (DVU.fromList [0xbe, 0xee, 0x66]) msys
      memvec = mReadN_ msys' 0x11 7
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
      msys    = Fold.foldl writeRAM (M.mkRAMRegion ramBase ramSize (mempty :: TestMemSystem)) pairs
      memvec  = mReadN_ msys ramBase ramSize
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
          msys    = DVU.foldl' writeRAM ramMsys writes
          memvec  = mReadN_ msys ramBase ramSize
          cmpvec  = DVU.update ramMvec (DVU.map (first fromIntegral) writes)
      in  M.sanityCheck msys && memvec == cmpvec

writeRAM :: (Integral addrType, DVU.Unbox wordType) =>
            M.MemorySystem addrType wordType
         -> (addrType, wordType)
         -> M.MemorySystem addrType wordType
writeRAM msys (addr, val) = M.mWrite addr val msys
{-# INLINABLE writeRAM #-}

testMemMappedDev :: (Int, TestMemSystem)
testMemMappedDev = M.mkDevRegion 0x100 0x101 mkTestDevice (mempty :: TestMemSystem)

test_MemMappedDeviceCreate :: TestParams -> Assertion
test_MemMappedDeviceCreate _args =
  let (didx, msys) = testMemMappedDev
      rlist        = map fst (M.regionList msys)
      nRegions     = M.countRegions msys == 1
      mRegions     = rlist == [I.IntervalCO 0x100 0x101]
      diagnose | didx /= 0
               = "Expected device index == 0"
               | not nRegions
               = "Expected one memory region" ++ show (M.countRegions msys)
               | not mRegions
               = "Region mismatch: " ++ show rlist
               | otherwise
               = "Huh?"
  in  assertBool diagnose (didx == 0 && nRegions && mRegions)

test_MemMappedDeviceReads :: TestParams -> Assertion
test_MemMappedDeviceReads _args =
  let (_didx, msys) = testMemMappedDev
      nElts         = 18
      readMem       = get >>= (\msys' -> let (x, msys'') = M.mRead msys' 0x100
                                         in  put msys'' >> return x)
      mReads         = evalState (replicateM nElts readMem) msys
      expected      = take nElts (iterate (+ 1) 19)
  in  assertBool ("Expected: " ++ show expected ++ ", got " ++ show mReads) (mReads == expected)
