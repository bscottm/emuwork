{- | 'MemorySystem' exercise module -}

module Main where

-- import           Control.Arrow                        (first)
import           Control.Monad                        (sequence, when)
-- import           Control.Monad.Trans.State.Strict     (evalState, execState, runState, state)
-- import           Data.Char                            (ord)
-- import qualified Data.Foldable                        as Fold
-- import Data.Functor.Identity (Identity)
-- import qualified Data.IntervalMap.Interval            as I
import Data.Bits
import           Data.List                            (and)
-- import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          (mempty)
-- import           Data.Vector.Unboxed                  (Vector, (!))
-- import qualified Data.Vector.Unboxed                  as DVU
-- import           Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Lens.Micro
-- import           System.IO                            (hPutStrLn, stderr)
-- import           System.Random                        (Random, StdGen, getStdGen, randomR, setStdGen)
import           Test.Framework                       (Test, defaultMain, testGroup)
-- import           Test.Framework.Options               (TestOptions' (..))
import           Test.Framework.Providers.HUnit       (testCase)
-- import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertBool)
-- import           Test.QuickCheck                      (Large, NonNegative, Property, choose, forAll, getLarge, getNonNegative)
import Text.Printf

#if defined(TEST_DEBUG)
import           Debug.Trace
import           Text.Printf
#endif

import          Machine
import          Z80

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
    defaultMain z80ExecTests

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The tests...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

z80system :: Z80system Z80BaseSystem
z80system = z80generic & sysName .~ "Test Z80 generic system"
                       & sysAliases .~ []
                       & memory .~ (mempty :: MemorySystem Z80addr Z80word)
                          <> msysRAMRegion 0x7200 1024
                          <> msysRAMRegion 0x6100  256
                          <> msysRAMRegion 0x6300  256


z80ExecTests :: [Test]
z80ExecTests =
  [ testGroup "LD group"
    [ testCase "Reg8Reg8            " test_ldReg8Reg8
    ]
  ]

z80initialCPU :: Z80system Z80BaseSystem
z80initialCPU = z80system & processor . cpu . regs .~ updRegs
  where
    z80regs = z80system ^. processor . cpu . regs
    updRegs = z80regs
                  & z80accum .~ 0xa5
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
                  & z80iyl   .~ 0x7b

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
        putStrLn "Expected:"
        printRegs leftRegs
        putStrLn "Got:"
        printRegs rightRegs
    return (leftRegs == rightRegs)
  where
    leftRegs  = leftSys ^. processor . cpu . regs
    rightRegs = rightSys ^. processor . cpu . regs

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

ordinaryReg8 :: [(Z80reg8, ASetter Z80registers Z80registers Z80word Z80word, Z80word, Text)]
ordinaryReg8 = [ (A, z80accum, 0x5a, "A")
               , (B, z80breg,  0x3b, "B")
               , (C, z80creg,  0x0c, "B")
               , (D, z80dreg,  0x8d, "D")
               , (E, z80ereg,  0x2e, "E")
               , (H, z80hreg,  0x27, "H")
               , (L, z80lreg,  0xc2, "L")
               ]

test_ldReg8Reg8 :: Assertion
test_ldReg8Reg8 =
  do
    retval <- testReg8Loads
    assertBool "" (and retval)
  where
    testReg8Loads = sequence [testReg8Load dst src | dst <- ordinaryReg8, src <- ordinaryReg8]
    testReg8Load (dstReg, dstSetter, _dstVal, dstText) (srcReg, srcSetter, srcVal, srcText) =
      compareRegs z80expected z80' (T.pack (printf "LD %s, %s (0x%02x)" dstText srcText srcVal))
      where
        z80'        = z80instructionExecute (DecodedInsn 0x1003 (LD (Reg8Reg8 dstReg srcReg))) z80test
        z80test     = z80initialCPU & processor . cpu . regs . srcSetter .~ srcVal
        z80expected = z80test    & processor . cpu . regs . dstSetter .~ srcVal
