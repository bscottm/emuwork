module Z80.Tests.Execute.ALUops where

import Data.Bits
import Data.Text as T
import           Lens.Micro.Platform
import           Test.HUnit                           (Assertion)
import Text.Printf

import Machine
import Z80

import Z80.Tests.Execute.Utils
import Z80.Tests.Execute.TestData
import Z80.Tests.Execute.QuickInstances()


test_subReg8 :: Assertion
test_subReg8 = assertBoolMessages [ runTest reg8 elt1 elt2
                                  | reg8 <- [A, B, C, D, E, H, L, IXh, IXl, IYh, IYl]
                                  , elt1 <- [0..0xff] :: [Z80word]
                                  , elt2 <- [0..0xff] :: [Z80word]
                                  ]
  where
    runTest reg8 val1 val2
      | reg8 == A
      , gotVal == 0
      = T.empty
      | gotVal == val1 - val2
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) (val1 - val2) gotVal
      where
        ins = SUB8 . ALUAcc . ALUreg8 $ reg8
        gotVal = accumResult reg8 val1 val2 (SUB8 . ALUAcc)


test_andReg8 :: Assertion
test_andReg8 = assertBoolMessages [ runTest reg8 elt1 elt2
                                  | reg8 <- [A, B, C, D, E, H, L, IXh, IXl, IYh, IYl]
                                  , elt1 <- [0..0xff] :: [Z80word]
                                  , elt2 <- [0..0xff] :: [Z80word]
                                  ]
  where
    runTest reg8 val1 val2
      | reg8 == A
      , gotVal == val1
      = T.empty
      | gotVal == val1 .&. val2
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) (val1 .&. val2) gotVal
      where
        ins = AND . ALUreg8 $ reg8
        gotVal = accumResult reg8 val1 val2 AND


test_xorReg8 :: Assertion
test_xorReg8 = assertBoolMessages [ runTest reg8 elt1 elt2
                                  | reg8 <- [A, B, C, D, E, H, L, IXh, IXl, IYh, IYl]
                                  , elt1 <- [0..0xff] :: [Z80word]
                                  , elt2 <- [0..0xff] :: [Z80word]
                                  ]
  where
    runTest reg8 val1 val2
      | reg8 == A
      , gotVal == 0
      = T.empty
      | gotVal == val1 `xor` val2
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) (val1 `xor` val2) gotVal
      where
        ins = XOR . ALUreg8 $ reg8
        gotVal = accumResult reg8 val1 val2 XOR


test_orReg8 :: Assertion
test_orReg8 = assertBoolMessages [ runTest reg8 elt1 elt2
                                  | reg8 <- [A, B, C, D, E, H, L, IXh, IXl, IYh, IYl]
                                  , elt1 <- [0..0xff] :: [Z80word]
                                  , elt2 <- [0..0xff] :: [Z80word]
                                  ]
  where
    runTest reg8 val1 val2
      | reg8 == A
      , gotVal == val1
      = T.empty
      | gotVal == val1 .|. val2
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) (val1 .|. val2) gotVal
      where
        ins = OR . ALUreg8 $ reg8
        gotVal = accumResult reg8 val1 val2 OR


accumResult
  :: Z80reg8
  -> Z80word
  -> Z80word
  -> (OperALU -> Z80instruction)
  -> Z80word
accumResult reg accum val ins = z80registers testSys ^. z80accum
  where
    initialSys
      | reg == A
      = z80system & processor . cpu . regs .~
                   ( z80registers z80system
                     & z80accum .~ accum
                   )
      | otherwise
      = z80system & processor . cpu . regs .~
                   ( z80registers z80system
                     & z80accum .~ accum
                     & z80Reg8Lens reg .~ val
                   )
    testSys     = z80instructionExecute (DecodedInsn 0x1000 (ins . ALUreg8 $ reg)) initialSys
