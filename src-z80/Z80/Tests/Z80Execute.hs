{- | Z80 instruction execution exercise module -}

module Main where

-- import           Control.Arrow                        (first)
import           Control.Monad                        (replicateM, sequence, when)
import           Control.Monad.Trans.State.Strict     (execState, runState, state)
-- import           Data.Char                            (ord)
-- import qualified Data.Foldable                        as Fold
-- import Data.Functor.Identity (Identity)
-- import qualified Data.IntervalMap.Interval            as I
import Data.Bits
import           Data.List                            (and)
-- import           Data.Maybe                           (fromMaybe)
-- import           Data.Monoid                          (mempty)
-- import           Data.Vector.Unboxed                  (Vector, (!))
import qualified Data.Vector.Unboxed                  as DVU
-- import           Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Lens.Micro
-- import           System.IO                            (hPutStrLn, stderr)
import           System.Random                        (Random, StdGen, getStdGen, randomR, setStdGen)
import           Test.Framework                       (Test, defaultMain, testGroup)
-- import           Test.Framework.Options               (TestOptions' (..))
import           Test.Framework.Providers.HUnit       (testCase)
-- import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertBool)
-- import           Test.QuickCheck                      (Large, NonNegative, Property, choose, forAll, getLarge, getNonNegative)
import Text.Printf

#if defined(TEST_DEBUG)
import           Debug.Trace
#endif

import          Machine
import          Z80

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
  do
    stdGen <- getStdGen
    let ((mem0x7200, mem0x6100, mem0x6200), stdGen') =
          runState ((,,) <$> state (finiteRandList (0, 0xff) 1024 :: (StdGen -> ([Z80word], StdGen)))
                         <*> state (finiteRandList (0, 0xff)  256 :: (StdGen -> ([Z80word], StdGen)))
                         <*> state (finiteRandList (0, 0xff)  256 :: (StdGen -> ([Z80word], StdGen)))
                   )
                   stdGen
        sysSeq = sequence [ stateSysMWriteN 0x7200 (DVU.fromList mem0x7200)
                          , stateSysMWriteN 0x6100 (DVU.fromList mem0x6100)
                          , stateSysMWriteN 0x6200 (DVU.fromList mem0x6200)
                          ]
    let testOptions =
          TestOptions
            { z80randMem = execState sysSeq z80initialCPU
            }
    setStdGen stdGen'
    defaultMain (z80ExecTests testOptions)

newtype TestOptions =
  TestOptions
    { z80randMem :: Z80system Z80BaseSystem
    }

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The tests...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

z80system :: Z80system Z80BaseSystem
z80system = z80generic & sysName .~ "Test Z80 generic system"
                       & sysAliases .~ []
                       & memory .~ msysRAMRegion 0x7200 1024
                          <> msysRAMRegion 0x6100  256
                          <> msysRAMRegion 0x6300  256

-- | The test groups and cases
z80ExecTests
  :: TestOptions
  -> [Test]
z80ExecTests opts =
  [ testGroup "LD group"
    [ testCase "Reg8Reg8            " (test_ldReg8Reg8 opts)
    ]
  ]

z80initialCPU :: Z80system Z80BaseSystem
z80initialCPU = z80system & processor . cpu . regs .~
                  (z80registers z80system &
                      z80accum .~ 0xa5
                    & z80breg  .~ 0xb3
                    & z80creg  .~ 0xc0
                    & z80dreg  .~ 0xd8
                    & z80ereg  .~ 0xe2
                    -- HL should be in the 0x7200 RAM area
                    & z80hreg  .~ 0x72
                    & z80lreg  .~ 0x2c
                    -- IX should be in the 0x6300 RAM area
                    & z80ixh   .~ 0x61
                    & z80ixl   .~ 0x5d
                    -- IY should be in the 0x6300 RAM area
                    & z80iyh   .~ 0x63
                    & z80iyl   .~ 0x7b)

initialHLAddr, initialIXAddr, initialIYAddr :: Z80addr
initialHLAddr = 0x722c
initialIXAddr = 0x615d
initialIYAddr = 0x637b

-- | Compare two systems' registers: 'leftRegs' are the expected registers, 'rightRegs' are the actual values. If they
-- don't match, then the contents of both are printed to 'stdout'.
compareRegs
  :: Z80system sysType
  -> Z80system sysType
  -> Text
  -> IO Bool
compareRegs leftSys rightSys banner =
  do
    when (leftRegs /= rightRegs) $
      do
        TIO.putStrLn banner
        TIO.putStrLn "Expected:"
        printRegs leftRegs
        TIO.putStrLn "Got:"
        printRegs rightRegs
    return (leftRegs == rightRegs)
  where
    leftRegs  = z80registers leftSys
    rightRegs = z80registers rightSys

-- | Compare a memory location with an expected value, print an error message if they don't match.
compareMem
  :: Z80addr
  -> Z80word
  -> Z80system sysType
  -> Text
  -> IO Bool
compareMem addr expectedVal z80sys banner =
  do
    let ctnt = fst $ sysMRead addr z80sys
    when (ctnt /= expectedVal) $
      do
        TIO.putStrLn banner
        TIO.putStrLn (T.pack (printf "compareMem Expected: 0x%02x, got 0x%02x" expectedVal ctnt))
    return (ctnt == expectedVal)

-- | Dump registers to stdout.
printRegs
  :: Z80registers
  -> IO ()
printRegs zregs =
  printf "A: 0x%02x B: 0x%02x C: 0x%02x D: 0x%02x E: 0x%02x HL: 0x%04x IX: 0x%04x IY: 0x%04x SP: 0x%04x\n"
    (zregs ^. z80accum)
    (zregs ^. z80breg)
    (zregs ^. z80creg)
    (zregs ^. z80dreg)
    (zregs ^. z80ereg)
    (((fromIntegral (zregs ^. z80hreg) :: Z80addr) `shiftL` 8) .|. (fromIntegral (zregs ^. z80lreg) :: Z80addr) .&. 0x00ff)
    (((fromIntegral (zregs ^. z80ixh) :: Z80addr) `shiftL` 8)  .|. (fromIntegral (zregs ^. z80ixl)  :: Z80addr) .&. 0x00ff)
    (((fromIntegral (zregs ^. z80iyh) :: Z80addr) `shiftL` 8)  .|. (fromIntegral (zregs ^. z80iyl)  :: Z80addr) .&. 0x00ff)
    (zregs ^. z80sp)

-- | Generate finite sized random lists.
finiteRandList :: (Random a, Num a) => (a, a) -> Int -> StdGen -> ([a], StdGen)
finiteRandList range lim = runState (replicateM lim (state (randomR range)))

-- | The ordinary 8-bit registers (does not include the indirect (HL), (IX|IY+d) memory references)
ordinaryReg8 :: [(Z80reg8, ASetter Z80registers Z80registers Z80word Z80word, Getting Z80word Z80registers Z80word, Z80word, Text)]

ordinaryReg8 = [ (A,   z80accum, z80accum, 0x5a, "A")
               , (B,   z80breg,  z80breg,  0x3b, "B")
               , (C,   z80creg,  z80creg,  0x0c, "B")
               , (D,   z80dreg,  z80dreg,  0x8d, "D")
               , (E,   z80ereg,  z80ereg,  0x2e, "E")
               , (H,   z80hreg,  z80hreg,  0x27, "H")
               , (L,   z80lreg,  z80lreg,  0xc2, "L")
               , (IXh, z80ixh,   z80ixh,   0x61, "IXh")
               , (IXl, z80ixl,   z80ixl,   0x5d, "IXl")
               , (IYh, z80iyh,   z80iyh,   0x63, "IYh")
               , (IYl, z80iyl,   z80iyl,   0x7b, "IYl")
               ]

-- | Test the 8-bit-to-8-bit loads (LD A, B; LD B, (HL); LD H, (IX+3) ...)
test_ldReg8Reg8
  :: TestOptions
  -> Assertion
test_ldReg8Reg8 opts =
  do
    reg8direct <- testReg8DirectLoads
    assertBool "Reg8Reg8 failed." (and reg8direct)
    reg8Indirect <- testReg8Indirect
    assertBool "Reg8 HL/IX/IY indirects failed." (and reg8Indirect)
  where
    testReg8DirectLoads = sequence [testReg8Load dst src | dst <- ordinaryReg8, src <- ordinaryReg8]
    testReg8Load (dstReg, dstSetter, _dstGetter, _dstVal, dstText) (srcReg, srcSetter, _srcGetter, srcVal, srcText) =
      compareRegs z80expected z80' (T.pack (printf "LD %s, %s (0x%02x)" dstText srcText srcVal))
      where
        z80'        = z80instructionExecute (DecodedInsn 0x1003 (LD (Reg8Reg8 dstReg srcReg))) z80test
        z80test     = z80initialCPU & processor . cpu . regs . srcSetter .~ srcVal
        z80expected = z80test       & processor . cpu . regs . dstSetter .~ srcVal

    z80indirectSys  = z80randMem opts

    testReg8Indirect =
      do
        stdGen   <- getStdGen
        let (ixOffset, stdGen')  = randomR (-128, 127) stdGen
        let (iyOffset, stdGen'') = randomR (-128, 127) stdGen'
        setStdGen stdGen''
        hlLoads  <- sequence [testHLIndirectLoads  dst hlIndirectLoad  0x00     "LD %s, (HL)"                           initialHLAddr | dst <- ordinaryReg8]
        hlStores <- sequence [testHLIndirectStores src hlIndirectStore 0x00     "LD (HL), %s"                           initialHLAddr | src <- ordinaryReg8]
        ixLoads  <- sequence [testHLIndirectLoads  dst ixIndirectLoad  ixOffset (printf "LD %%s, (IX+0x%02x)" ixOffset) initialIXAddr | dst <- ordinaryReg8]
        ixStores <- sequence [testHLIndirectStores src ixIndirectStore ixOffset (printf "LD (IX+0x%02x), %%s" ixOffset) initialIXAddr | src <- ordinaryReg8]
        iyLoads  <- sequence [testHLIndirectLoads  dst iyIndirectLoad  iyOffset (printf "LD %%s, (IY+0x%02x)" iyOffset) initialIYAddr | dst <- ordinaryReg8]
        iyStores <- sequence [testHLIndirectStores src iyIndirectStore iyOffset (printf "LD (IY+0x%02x), %%s" iyOffset) initialIYAddr | src <- ordinaryReg8]
        return (hlLoads ++ hlStores ++ ixLoads ++ ixStores ++ iyLoads ++ iyStores)

    hlIndirectLoad  reg _idxOffset = LD (Reg8Reg8 reg HLindirect)
    hlIndirectStore reg _idxOffset = LD (Reg8Reg8 HLindirect reg)
    ixIndirectLoad  reg  idxOffset = LD (Reg8Reg8 reg (IXindirect idxOffset))
    ixIndirectStore reg  idxOffset = LD (Reg8Reg8 (IXindirect idxOffset) reg)
    iyIndirectLoad  reg  idxOffset = LD (Reg8Reg8 reg (IYindirect idxOffset))
    iyIndirectStore reg  idxOffset = LD (Reg8Reg8 (IYindirect idxOffset) reg)

    testHLIndirectLoads (dstReg, dstSetter, _dstGetter, _dstVal, dstText) loadFunc idxOffset bannerStr memAddr =
      compareRegs z80expected z80' (T.pack (printf bannerStr dstText))
      where
        hlval        = fst $ sysMRead (memAddr + fromIntegral idxOffset) z80indirectSys
        z80'         = z80instructionExecute (DecodedInsn 0x1005 (loadFunc dstReg idxOffset)) z80indirectSys
        z80expected  = z80indirectSys & processor . cpu . regs . dstSetter .~ hlval

    testHLIndirectStores (srcReg, _srcSetter, srcGetter, _srcVal, srcText) loadFunc idxOffset bannerStr memAddr =
      compareMem (memAddr + fromIntegral idxOffset) expectedVal z80' (T.pack (printf bannerStr srcText))
      where
        z80'         = z80instructionExecute (DecodedInsn 0x1005 (loadFunc srcReg idxOffset)) z80indirectSys
        expectedVal  = z80indirectSys ^. processor . cpu . regs . srcGetter
