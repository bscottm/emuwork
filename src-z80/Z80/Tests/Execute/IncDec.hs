module Z80.Tests.Execute.IncDec where

import           Control.Monad                        (sequence, when)
import Data.Bits
import Data.Foldable as Fold
import Data.Word
import Data.Text as T
import           Lens.Micro.Platform
import System.IO
import           System.Random                        (getStdGen, randomR, setStdGen)
import           Test.HUnit                           (Assertion, assertBool)
import           Test.QuickCheck                      (Property, elements, forAllShow, conjoin)
import Text.Printf

import Machine
import Z80

import Z80.Tests.Execute.TestData
import Z80.Tests.Execute.Utils

-- import Debug.Trace


prop_inc8 :: Property
prop_inc8 = conjoin [ forAllShow (elements ([0..255] :: [Word8]))
                                 (inc8_diagnose     ins getLens setLens)
                                 (incDec8Test (+ 1) ins getLens setLens)
                    | (ins, getLens, setLens) <-
                      [ (INC A,   z80accum, z80accum)
                      , (INC B,   z80breg,  z80breg)
                      , (INC C,   z80creg,  z80creg)
                      , (INC D,   z80dreg,  z80dreg)
                      , (INC E,   z80ereg,  z80ereg)
                      , (INC H,   z80hreg,  z80hreg)
                      , (INC L,   z80lreg,  z80lreg)
                      , (INC IXh, z80ixh,   z80ixh)
                      , (INC IXl, z80ixl,   z80ixl)
                      , (INC IYh, z80iyh,   z80iyh)
                      , (INC IYl, z80iyl,   z80iyl)
                        ]
                    ]
  where
    inc8_diagnose ins getLens setLens val = printf "%s: Expected 0x%04x, got 0x%04x" insn val gotVal
      where
        gotVal = incDec8TestSys ins getLens setLens val
        insn   = z80ShortInsnFormat ins


prop_dec8 :: Property
prop_dec8 = conjoin [ forAllShow (elements ([0..255] :: [Word8]))
                                 (dec8_diagnose            ins getLens setLens)
                                 (incDec8Test (subtract 1) ins getLens setLens)
                    | (ins, getLens, setLens) <-
                      [ (DEC A,   z80accum, z80accum)
                      , (DEC B,   z80breg,  z80breg)
                      , (DEC C,   z80creg,  z80creg)
                      , (DEC D,   z80dreg,  z80dreg)
                      , (DEC E,   z80ereg,  z80ereg)
                      , (DEC H,   z80hreg,  z80hreg)
                      , (DEC L,   z80lreg,  z80lreg)
                      , (DEC IXh, z80ixh,   z80ixh)
                      , (DEC IXl, z80ixl,   z80ixl)
                      , (DEC IYh, z80iyh,   z80iyh)
                      , (DEC IYl, z80iyl,   z80iyl)
                        ]
                    ]
  where
    dec8_diagnose ins getLens setLens val = printf "%s: Expected 0x%04x, got 0x%04x" insn val gotVal
      where
        gotVal = incDec8TestSys ins getLens setLens val
        insn   = z80ShortInsnFormat ins


incDec8Test
  :: (Z80word -> Z80word)
  -> Z80instruction
  -> Getting Z80word Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> Z80word
  -> Bool
incDec8Test op ins getLens setLens val = op val == incDec8TestSys ins getLens setLens val

incDec8TestSys
  :: Z80instruction
  -> Getting Z80word Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> Z80word
  -> Z80word
incDec8TestSys ins getLens setLens val = z80registers testSys ^. getLens
  where
    testSys     = z80instructionExecute (DecodedInsn 0x1000 ins) initialSys
    initialSys  = z80system & processor . cpu . regs .~ (z80registers z80system & setLens .~ val)


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
                hPutStrLn stderr $ Fold.concat [banner, " -> got " , (show expected), ", expected ", (show got)]
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


prop_inc16 :: Property
prop_inc16 = conjoin [ forAllShow (elements ([0..65535] :: [Word16]))
                                  (inc16_diagnose     ins hiGetLens hiSetLens loGetLens loSetLens)
                                  (incDec16Test (+ 1) ins hiGetLens hiSetLens loGetLens loSetLens)
                     | (ins, hiGetLens, hiSetLens, loGetLens, loSetLens) <-
                         [ (INC16 (RPair16 BC), z80breg, z80breg, z80creg, z80creg)
                         , (INC16 (RPair16 DE), z80dreg, z80dreg, z80ereg, z80ereg)
                         , (INC16 (RPair16 HL), z80hreg, z80hreg, z80lreg, z80lreg)
                         , (INC16 (RPair16 IX), z80ixh,  z80ixh,  z80ixl,  z80ixl)
                         , (INC16 (RPair16 IY), z80iyh,  z80iyh,  z80iyl,  z80iyl)
                         ]
                     ]
  where
    inc16_diagnose ins hiGetLens hiSetLens loGetLens loSetLens val = printf "%s: Expected 0x%04x, got 0x%04x" insn val gotVal
      where
        gotVal = incDec16TestSys ins hiGetLens hiSetLens loGetLens loSetLens val
        insn   = z80ShortInsnFormat ins


prop_dec16 :: Property
prop_dec16 = conjoin [ forAllShow (elements ([0..65535] :: [Word16]))
                                  (dec16_diagnose            ins hiGetLens hiSetLens loGetLens loSetLens)
                                  (incDec16Test (subtract 1) ins hiGetLens hiSetLens loGetLens loSetLens)
                     | (ins, hiGetLens, hiSetLens, loGetLens, loSetLens) <-
                         [ (DEC16 (RPair16 BC), z80breg, z80breg, z80creg, z80creg)
                         , (DEC16 (RPair16 DE), z80dreg, z80dreg, z80ereg, z80ereg)
                         , (DEC16 (RPair16 HL), z80hreg, z80hreg, z80lreg, z80lreg)
                         , (DEC16 (RPair16 IX), z80ixh,  z80ixh,  z80ixl,  z80ixl)
                         , (DEC16 (RPair16 IY), z80iyh,  z80iyh,  z80iyl,  z80iyl)
                         ]
                     ]
  where
    dec16_diagnose ins hiGetLens hiSetLens loGetLens loSetLens val = printf "%s: Expected 0x%04x, got 0x%04x" insn val gotVal
      where
        gotVal = incDec16TestSys ins hiGetLens hiSetLens loGetLens loSetLens val
        insn   = z80ShortInsnFormat ins

incDec16Test
  :: (Z80addr -> Z80addr)
  -> Z80instruction
  -> Getting Z80word Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> Getting Z80word Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> Z80addr
  -> Bool
incDec16Test op ins hiGetLens hiSetLens loGetLens loSetLens val = op val == testVal
  where
    testVal = incDec16TestSys ins hiGetLens hiSetLens loGetLens loSetLens val

incDec16TestSys
  :: Z80instruction
  -> Getting Z80word Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> Getting Z80word Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> Z80addr
  -> Z80addr
incDec16TestSys ins hiGetLens hiSetLens loGetLens loSetLens val = testHiVal .|. testLoVal
  where
    testHiVal   = (fromIntegral (z80registers testSys ^. hiGetLens) :: Z80addr) `shiftL` 8
    testLoVal   = fromIntegral (z80registers testSys ^. loGetLens) :: Z80addr
    testSys     = z80instructionExecute (DecodedInsn 0x1000 ins) initialSys
    initialSys  = z80system & processor . cpu . regs .~
                  (z80registers z80system
                   & hiSetLens .~ hiVal
                   & loSetLens .~ loVal
                  )
    loVal = fromIntegral (val .&. 0xff) :: Word8
    hiVal = fromIntegral (val `shiftR` 8) :: Word8
