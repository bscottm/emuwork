{- | Z80 instruction execution exercise module -}

module Main where

import           Control.Monad                        (replicateM, sequence)
import           Control.Monad.Trans.State.Strict     (execState, runState, state)
import qualified Data.Vector.Unboxed                  as DVU
import           System.Random                        (Random, StdGen, getStdGen, randomR, setStdGen)
import           Test.Framework                       (Test, defaultMain, testGroup {-, plusTestOptions-})
-- import           Test.Framework.Options               (TestOptions'(..))
import           Test.Framework.Providers.HUnit       (testCase)
-- import           Test.Framework.Providers.QuickCheck2 (testProperty)

#if defined(TEST_DEBUG)
import           Debug.Trace
#endif

import          Machine
import          Z80

-- Internal test modules:
import          Z80.Tests.Execute.TestData
import          Z80.Tests.Execute.LoadStore
import          Z80.Tests.Execute.IncDec
import          Z80.Tests.Execute.ALUops

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Driver...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

main :: IO ()
main =
  do
    stdGen <- getStdGen
    let ((mem0x7200, mem0x6100, mem0x6300), stdGen') =
          runState ((,,) <$> state (finiteRandList (0, 0xff) 1024 :: (StdGen -> ([Z80word], StdGen)))
                         <*> state (finiteRandList (0, 0xff)  256 :: (StdGen -> ([Z80word], StdGen)))
                         <*> state (finiteRandList (0, 0xff)  256 :: (StdGen -> ([Z80word], StdGen)))
                   )
                   stdGen
        sysSeq = sequence [ stateSysMWriteN 0x7200                (DVU.fromList mem0x7200)
                          , stateSysMWriteN (initialIXAddr - 128) (DVU.fromList mem0x6100)
                          , stateSysMWriteN (initialIYAddr - 128) (DVU.fromList mem0x6300)
                          ]
    let testOptions =
          TestParams
            { z80randMem = execState sysSeq z80initialCPU
            }
    setStdGen stdGen'
    defaultMain (z80ExecTests testOptions)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The tests...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | The test groups and cases
z80ExecTests
  :: TestParams
  -> [Test]
z80ExecTests opts =
  [ testGroup "LD group"
    [ testCase "Reg8Reg8                      " (test_ldReg8Reg8      opts)
    , testCase "Reg8Imm                       " (test_ldReg8Imm       opts)
    , testCase "(BC), (DE), (imm) indirect    " (test_ldOtherIndirect opts)
    , testCase "I, R register specials        " (test_ldSpecials      opts)
    , testCase "Reg16 immediates              " (test_ldReg16Imm      opts)
    , testCase "Reg16 indirect loads          " (test_ldReg16IndLoad  opts)
    , testCase "Reg16 indirect stores         " (test_ldReg16IndStore opts)
    ],
    testGroup "INC/DEC"
    [ testCase "8-bit register increment      " test_incReg8
    , testCase "8-bit register decrement      " test_decReg8
    , testCase "Indirect Reg8 inc/dec         " (test_incDecIndReg8   opts)
    , testCase "Increment/Decrement Reg8 flags" (test_incDecReg8CC    opts)
    , testCase "16-bit register increment     " test_incReg16
    , testCase "16-bit register decrement     " test_decReg16
    , testCase "8-bit register SUB            " test_subReg8
    , testCase "8-bit register AND            " test_andReg8
    , testCase "8-bit register XOR            " test_xorReg8
    , testCase "8-bit register OR             " test_orReg8
    ]
  ]
{-  where
    -- 'mempty' is really the TestOptions record instantiated with all of the
    -- members set to Nothing.
    mkLargeTests nTests = mempty { topt_maximum_generated_tests = Just nTests
                                 , topt_maximum_unsuitable_generated_tests = Just (nTests * 5)
                                 }
-}

-- | Generate finite sized random lists.
finiteRandList :: (Random a, Num a) => (a, a) -> Int -> StdGen -> ([a], StdGen)
finiteRandList range lim = runState (replicateM lim (state (randomR range)))
