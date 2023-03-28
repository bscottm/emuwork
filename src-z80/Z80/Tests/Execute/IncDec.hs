module Z80.Tests.Execute.IncDec where

import           Control.Monad       (when)

import           Data.Bits
import           Data.Foldable       as Fold
import           Data.Text           as T
import           Data.Word

import           Lens.Micro.Platform

import           System.IO
import           System.Random       (getStdGen, randomR, setStdGen)

import           Test.HUnit          (Assertion, assertBool)

import           Text.Printf

import Machine
import Z80

import Z80.Tests.Execute.TestData
import Z80.Tests.Execute.Utils

-- import Debug.Trace

test_incReg8 :: Assertion
test_incReg8 = assertBoolMessages [ runTest reg8 elt
                                  | reg8 <- [A, B, C, D, E, H, L, IXh, IXl, IYh, IYl]
                                  , elt  <- [0..0xff] :: [Z80word]
                                  ]
  where
    runTest reg8 val
      | incDec8Test (+ 1) ins (z80Reg8Lens reg8) val
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) val gotVal
      where
        ins = INC reg8
        gotVal = incDec8TestSys ins (z80Reg8Lens reg8) val


test_decReg8 :: Assertion
test_decReg8 = assertBoolMessages [ runTest reg8 elt
                                  | reg8 <- [A, B, C, D, E, H, L, IXh, IXl, IYh, IYl]
                                  , elt  <- [0..0xff] :: [Z80word]
                                  ]
  where
    runTest reg8 val
      | incDec8Test (subtract 1) ins (z80Reg8Lens reg8) val
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) val gotVal
      where
        ins = DEC reg8
        gotVal = incDec8TestSys ins (z80Reg8Lens reg8) val


incDec8Test
  :: (Z80word -> Z80word)
  -> Z80instruction
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Z80word
  -> Bool
incDec8Test op ins regLens val = op val == incDec8TestSys ins regLens val


incDec8TestSys
  :: Z80instruction
  -> Lens Z80registers Z80registers Z80word Z80word
  -> Z80word
  -> Z80word
incDec8TestSys ins regLens val = z80registers testSys ^. regLens
  where
    testSys     = z80instructionExecute (DecodedInsn 0x1000 ins) initialSys
    initialSys  = z80system & processor . cpu . regs .~ (z80registers z80system & regLens .~ val)


test_incDecIndReg8
  :: TestParams
  -> Assertion
test_incDecIndReg8 opts =
  do
    stdGen <- getStdGen
    let (ixOffset, stdGen')  = randomR (-128, 127) stdGen
        (iyOffset, stdGen'') = randomR (-128, 127) stdGen'
        (offset, stdGen''')  = randomR (0, 1023) stdGen''
        indirectAddr         = 0x7200 + offset
    setStdGen stdGen'''

    let incHLindirect = indirectTest "(HL)" HLindirect indirectAddr (+ 1) INC
        decHLindirect = indirectTest "(HL)" HLindirect indirectAddr (subtract 1) DEC
        incIXindirect = indirectTest "(IX+d)" (IXindirect ixOffset) (initialIXAddr + fromIntegral ixOffset) (+ 1) INC
        decIXindirect = indirectTest "(IX+d)" (IXindirect ixOffset) (initialIXAddr + fromIntegral ixOffset) (subtract 1) DEC
        incIYindirect = indirectTest "(IY+d)" (IYindirect iyOffset) (initialIYAddr + fromIntegral iyOffset) (+ 1) INC
        decIYindirect = indirectTest "(IY+d)" (IYindirect iyOffset) (initialIYAddr + fromIntegral iyOffset) (subtract 1) DEC

    assertBoolMessages [ incHLindirect, decHLindirect
                       , incIXindirect, decIXindirect
                       , incIYindirect, decIYindirect
                       ]
  where
    indirectTest regStr indirectReg addr op ins =
      T.pack $
        if expectedVal == gotVal
        then ""
        else printf "INC/DEC %s: Expected 0x%02x, got 0x%02x\n" (regStr :: Text) expectedVal gotVal
      where
        z80indirectSys =
          z80randMem opts & processor . cpu .regs .~
            ( z80registers (z80randMem opts) 
                & z80hreg .~ fromIntegral (addr `shiftR` 8)
                & z80lreg .~ fromIntegral (addr .&. 0xff))
        expectedVal = op . fst $ sysMRead addr z80indirectSys
        testSys = z80instructionExecute (DecodedInsn 0x1002 (ins indirectReg)) z80indirectSys
        gotVal = fst $ sysMRead addr testSys


test_incDecReg8CC
  :: TestParams
  -> Assertion
test_incDecReg8CC _opts =
  do
    result <- sequence [test_flags testVal
                       | testVal <- [ (INC, 0xff, False,  True, False,  True, False)
                                    , (DEC, 0x00, True,  False, False,  True,  True)
                                    , (INC, 0x01, False, False, False, False, False)
                                    , (INC, 0x49, False, False, True,  False, False)
                                    ]
                       ]
    assertBool "INC/DEC condition codes fail." (and result)
  where
    test_flags :: (Z80reg8 -> Z80instruction, Word8, Bool, Bool, Bool, Bool, Bool)
               -> IO Bool
    test_flags (ins, val, sign, zero, hcarry, overflow, negval) =
      do
        let insn = z80ShortInsnFormat (ins A)
            newval  = testCPU ^. regs . z80accum
        test_result <- sequence [ check flagSign      sign     (printf "%s sign   [val 0x%02x newval 0x%02x]" insn val newval)
                                , check flagZero      zero     (printf "%s zero   [val 0x%02x newval 0x%02x]" insn val newval)
                                , check flagHalfCarry hcarry   (printf "%s half   [val 0x%02x newval 0x%02x]" insn val newval)
                                , check flagParOv     overflow (printf "%s ovf    [val 0x%02x newval 0x%02x]" insn val newval)
                                , check flagNFlag     negval   (printf "%s negval [val 0x%02x newval 0x%02x]" insn val newval)
                                ]
        return (and test_result)
      where
        -- check :: SimpleGetter Z80state Bool -> Bool -> String -> IO Bool
        check getter expected banner =
          do
            let got = testCPU ^. getter
            when (got /= expected) $
              do
                hPutStrLn stderr $ Fold.concat [banner, " -> got " , show expected, ", expected ", show got]
                printFlags "initial" (initialSys ^. processor . cpu)  
                printFlags "testCPU" testCPU
            return (got == expected)
        testSys = z80instructionExecute (DecodedInsn 0x1002 (ins A)) initialSys
        testCPU = testSys ^. processor . cpu
        initialSys =
          z80system & processor . cpu .~
            ( z80system ^. processor . cpu
              & regs          .~ (z80registers z80system & z80accum .~ val)
              & flagSign      .~ not sign
              & flagZero      .~ not zero
              & flagHalfCarry .~ not hcarry
              & flagParOv     .~ not overflow
              & flagNFlag     .~ not negval
            )


test_incReg16 :: Assertion
test_incReg16 = assertBoolMessages [ runTest reg16 elt
                                   | reg16 <- [ BC, DE, HL, IX, IY ]
                                   , elt   <- [0..65535] :: [Word16]
                                   ]
  where
    runTest reg16 val
      | incDec16Test (+ 1) INC16 reg16 val
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) val gotVal
      where
        ins = INC16 . RPair16 $ reg16
        gotVal = incDec16TestSys INC16 reg16 val


test_decReg16 :: Assertion
test_decReg16 = assertBoolMessages [ runTest reg16 elt
                                    | reg16 <- [ BC, DE, HL, IX, IY ]
                                    , elt   <- [0..65535] :: [Word16]
                                    ]
  where
    runTest reg16 val
      | incDec16Test (subtract 1) DEC16 reg16 val
      = T.empty
      | otherwise
      = T.pack $ printf "%s: Expected 0x%04x, got 0x%04x" (z80ShortInsnFormat ins) val gotVal
      where
        ins = DEC16 . RPair16 $ reg16
        gotVal = incDec16TestSys INC16 reg16 val


incDec16Test
  :: (Z80addr -> Z80addr)
  -> (RegPairSP -> Z80instruction)
  -> Z80reg16
  -> Z80addr
  -> Bool
incDec16Test op ins reg16 val = op val == testVal
  where
    testVal = incDec16TestSys ins reg16 val

incDec16TestSys
  :: (RegPairSP -> Z80instruction)
  -> Z80reg16
  -> Z80addr
  -> Z80addr
incDec16TestSys ins reg16 val = reg16get reg16 testSys
  where
    testSys     = z80instructionExecute (DecodedInsn 0x1000 (ins . RPair16 $ reg16)) initialSys
    initialSys  = reg16set val reg16 z80system
