{- | Z80 instruction execution exercise module -}

module Main where

import           Control.Monad                        (replicateM, sequence)
import           Control.Monad.Trans.State.Strict     (execState, runState, state)
import qualified Data.Vector.Unboxed                  as DVU
import           System.Random                        (Random, StdGen, getStdGen, randomR, setStdGen)
import           Test.Framework                       (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)

#if defined(TEST_DEBUG)
import           Debug.Trace
#endif

import          Machine
import          Z80

-- Internal test modules:
import          Z80.Tests.Execute.TestData
import          Z80.Tests.Execute.LoadStore
import          Z80.Tests.Execute.IncDec

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
          TestOptions
            { z80randMem = execState sysSeq z80initialCPU
            }
    setStdGen stdGen'
    defaultMain (z80ExecTests testOptions)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The tests...
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | The test groups and cases
z80ExecTests
  :: TestOptions
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
    [ testCase "Increment/Decrement Reg8      " (test_incDecReg8      opts)
    , testCase "Increment/Decrement Reg8 flags" (test_incDecReg8CC    opts)
    ]
  ]

-- | Generate finite sized random lists.
finiteRandList :: (Random a, Num a) => (a, a) -> Int -> StdGen -> ([a], StdGen)
finiteRandList range lim = runState (replicateM lim (state (randomR range)))
