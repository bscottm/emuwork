module Z80.Tests.Execute.IncDec where

import           Control.Monad                        (sequence, when)
import Data.Bits
import Data.Word
import Data.Text as T hiding (concat)
import           Lens.Micro.Platform
import System.IO
import           System.Random                        (getStdGen, randomR, setStdGen)
import           Test.HUnit                           (Assertion, assertBool)
import Text.Printf

import Machine
import Z80

import Z80.Tests.Execute.TestData
import Z80.Tests.Execute.Utils

test_incDecReg8
  :: TestOptions
  -> Assertion
test_incDecReg8 opts =
  do
    stdGen <- getStdGen
    let (ixOffset, stdGen')  = randomR (-128, 127) stdGen
        (iyOffset, stdGen'') = randomR (-128, 127) stdGen'
        (offset, stdGen''')  = randomR (0, 1023) stdGen''
        indirectAddr         = 0x7200 + offset
    setStdGen stdGen'''

    incResults    <- sequence [test_incDec reg (+ 1) INC | reg <- reg8TestData]
    decResults    <- sequence [test_incDec reg (subtract 1) DEC | reg <- reg8TestData]
    assertBool "INC/DEC Reg8" (and (incResults ++ decResults))

    incHLindirect <- indirectTest "(HL)" HLindirect indirectAddr (+ 1) INC
    decHLindirect <- indirectTest "(HL)" HLindirect indirectAddr (subtract 1) DEC
    assertBool "INC/DEC (HL)" (incHLindirect && decHLindirect)

    incIXindirect <- indirectTest "(IX+d)" (IXindirect ixOffset) (initialIXAddr + fromIntegral ixOffset) (+ 1) INC
    decIXindirect <- indirectTest "(IX+d)" (IXindirect ixOffset) (initialIXAddr + fromIntegral ixOffset) (subtract 1) DEC
    assertBool "INC/DEC (IX+disp)" (incIXindirect && decIXindirect)

    incIYindirect <- indirectTest "(IY+d)" (IYindirect iyOffset) (initialIYAddr + fromIntegral iyOffset) (+ 1) INC
    decIYindirect <- indirectTest "(IY+d)" (IYindirect iyOffset) (initialIYAddr + fromIntegral iyOffset) (subtract 1) DEC
    assertBool "INC/DEC (IY+disp)" (incIYindirect && decIYindirect)
  where
    test_incDec (dstReg, _dstSetter, dstGetter, _dstVal, dstText) op ins =
      do
        when (expectedVal /= testVal) $
          printf "INC/DEC %s: Expected 0x%02x, got 0x%02x\n" dstText expectedVal testVal
        return (expectedVal == testVal)
      where
        expectedVal = op (z80registers z80initialCPU ^. dstGetter)
        testSys     = z80instructionExecute (DecodedInsn 0x1000 (ins dstReg)) z80initialCPU
        testVal     = z80registers testSys ^. dstGetter

    indirectTest regStr indirectReg addr op ins =
      do
        when (expectedVal /= gotVal) $
          printf "INC/DEC %s: Expected 0x%02x, got 0x%02x\n" (regStr :: Text) expectedVal gotVal
        return (expectedVal == gotVal)
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
  :: TestOptions
  -> Assertion
test_incDecReg8CC _opts =
  do
    result <- sequence [test_flags testVal
                       | testVal <- [ (INC, 0xff, False,  True, False,  True, False,  True)
                                    , (DEC, 0x00, True,  False, False,  True,  True,  True)
                                    , (INC, 0x01, False, False, False, False, False, False)
                                    , (INC, 0x49, False, False, True,  False, False, False)
                                    ]
                       ]
    assertBool "INC/DEC condition codes fail." (and result)
  where
    test_flags :: (Z80reg8 -> Z80instruction, Word8, Bool, Bool, Bool, Bool, Bool, Bool)
               -> IO Bool
    test_flags (ins, val, sign, zero, hcarry, overflow, negval, carry) =
      do
        let insName = insnName (ins A)
            newval  = testCPU ^. regs . z80accum
        test_result <- sequence [ check flagSign      sign     (printf "%s sign   [val 0x%02x newval 0x%02x]" insName val newval)
                                , check flagZero      zero     (printf "%s zero   [val 0x%02x newval 0x%02x]" insName val newval)
                                , check flagHalfCarry hcarry   (printf "%s half   [val 0x%02x newval 0x%02x]" insName val newval)
                                , check flagParOv     overflow (printf "%s ovf    [val 0x%02x newval 0x%02x]" insName val newval)
                                , check flagNFlag     negval   (printf "%s negval [val 0x%02x newval 0x%02x]" insName val newval)
                                , check flagCarry     carry    (printf "%s carry  [val 0x%02x newval 0x%02x]" insName val newval)
                                ]
        return (and test_result)
      where
        check :: SimpleGetter Z80state Bool -> Bool -> String -> IO Bool
        check getter expected banner =
          do
            let got = testCPU ^. getter
            when (got /= expected) $
              do
                hPutStrLn stderr $ concat [banner, " -> got " , (show expected), ", expected ", (show got)]
                printFlags "initial" (initialSys ^. processor . cpu)  
                printFlags "testCPU" testCPU
            return (got == expected)
        insnName :: Z80instruction -> String
        insnName (INC _) = "INC"
        insnName (DEC _) = "DEC"
        insnName _       = "<unknown>"
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
              & flagCarry     .~ not carry
            )
