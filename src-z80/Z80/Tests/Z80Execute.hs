{- | Z80 instruction execution exercise module -}

module Main where

-- import           Control.Arrow                        as Arrow
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
import           Lens.Micro.Platform
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
                          -- IX indirect operations memory page (offsets are 8-bit signed)
                          <> msysRAMRegion (initialIXAddr - 128)  256
                          -- IY indirect operations memory page (-128 -> +127)
                          <> msysRAMRegion (initialIYAddr - 128)  256

-- | The test groups and cases
z80ExecTests
  :: TestOptions
  -> [Test]
z80ExecTests opts =
  [ testGroup "LD group"
    [ testCase "Reg8Reg8                  " (test_ldReg8Reg8      opts)
    , testCase "Reg8Imm                   " (test_ldReg8Imm       opts)
    , testCase "(BC), (DE), (imm) indirect" (test_ldOtherIndirect opts)
    , testCase "I, R register specials    " (test_ldSpecials      opts)
    , testCase "Reg16 immediates          " (test_ldReg16Imm      opts)
    , testCase "Reg16 indirect loads      " (test_ldReg16IndLoad  opts)
    , testCase "Reg16 indirect stores     " (test_ldReg16IndStore opts)
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
                    & z80ixh   .~ fromIntegral ((initialIXAddr `shiftR` 8) .&. 0xff)
                    & z80ixl   .~ fromIntegral (initialIXAddr .&. 0xff)
                    -- IY should be in the 0x6300 RAM area
                    & z80iyh   .~ fromIntegral ((initialIYAddr `shiftR` 8) .&. 0xff)
                    & z80iyl   .~ fromIntegral (initialIYAddr .&. 0xff))

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

-- | Compare a memory location with an expected byte value, print an error message if they don't match.
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

-- | Compare a memory location with an expected byte value, print an error message if they don't match.
compareMem16
  :: Z80addr
  -> Z80addr
  -> Z80system sysType
  -> Text
  -> IO Bool
compareMem16 addr expectedVal z80sys banner =
  do
    let ctnt = make16bit . fst $ sysMReadN addr 2 z80sys
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

-- | The ordinary 8-bit registers (does not include the indirect (HL), (IX|IY+d) memory references). The first tuple
-- argument is (obviously) the reigster. The second and third are 'Z80state' register setters and getters. You have to
-- have separate setters and getters, even though they are the same and combine the two operations, because the Haskell
-- type system will disallow using the same function in two different ways. The fourth argument is the test value.
ordinaryReg8
  :: [(Z80reg8
      , ASetter Z80registers Z80registers Z80word Z80word
      , Getting Z80word Z80registers Z80word
      , Z80word
      , Text
      )]
ordinaryReg8 =
  [ (A,   z80accum, z80accum, 0x5a, "A")
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

-- | The ordinary 16-bit registers used to test 16-bit load operations. The 'RegPairSP' argument is used for the 'LD'
-- instruction; the 'Z80reg16' is passed to 'reg16set' to set the expected value and the 'Z80addr' is the test value.
ordinaryReg16
  :: [(RegPairSP, Z80reg16, Z80system sysType -> Z80addr, Z80addr, Text)]
ordinaryReg16 =
  [ (RPair16 BC, BC, reg16get BC, 0x1234, "BC")
  , (RPair16 DE, DE, reg16get DE, 0x5678, "DE")
  , (RPair16 HL, HL, reg16get HL, 0x9abc, "HL")
  , (RPair16 IX, IX, reg16get IX, 0xdef0, "IX")
  , (RPair16 IY, IY, reg16get IY, 0x1f2e, "IY")
  ]

-- | Test the 8-bit-to-8-bit loads (LD A, B; LD B, (HL); LD H, (IX+3) ...)
test_ldReg8Reg8
  :: TestOptions
  -> Assertion
test_ldReg8Reg8 opts =
  do
    reg8direct <- sequence [testReg8Load dst src | dst <- ordinaryReg8, src <- ordinaryReg8]
    assertBool "Reg8Reg8 failed." (and reg8direct)
    reg8Indirect <- testReg8Indirect
    assertBool "Reg8 HL/IX/IY indirects failed." (and reg8Indirect)
  where
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
        hlLoads  <- sequence [testIndirectLoads  dst hlIndirectLoad initialHLAddr 0x00       "LD %s, (HL)"
                             | dst <- ordinaryReg8]
        hlStores <- sequence [testIndirectStores src hlIndirectStore initialHLAddr 0x00       "LD (HL), %s"                           
                             | src <- ordinaryReg8]
        ixLoads  <- sequence [testIndirectLoads  dst ixIndirectLoad initialIXAddr ixOffset
                                                 (printf "LD %%s, (IX+0x%02x) [addr = 0x%04x]"
                                                         ixOffset
                                                         (initialIXAddr + signExtend ixOffset))
                             | dst <- ordinaryReg8]
        ixStores <- sequence [testIndirectStores src ixIndirectStore initialIXAddr ixOffset
                                                 (printf "LD (IX+0x%02x), %%s [addr = 0x%04x]"
                                                         ixOffset (initialIXAddr + signExtend ixOffset))
                             | src <- ordinaryReg8]
        iyLoads  <- sequence [testIndirectLoads  dst iyIndirectLoad initialIYAddr iyOffset
                                                 (printf "LD %%s, (IY+0x%02x) [addr = 0x%04x]"
                                                         iyOffset (initialIYAddr + signExtend iyOffset))
                             | dst <- ordinaryReg8]
        iyStores <- sequence [testIndirectStores src iyIndirectStore initialIYAddr iyOffset
                                                 (printf "LD (IY+0x%02x), %%s [addr = 0x%04x]"
                                                         iyOffset (initialIYAddr + signExtend iyOffset))
                             | src <- ordinaryReg8]
        return (hlLoads ++ hlStores ++ ixLoads ++ ixStores ++ iyLoads ++ iyStores)

    hlIndirectLoad  reg _idxOffset = LD (Reg8Reg8 reg HLindirect)
    hlIndirectStore reg _idxOffset = LD (Reg8Reg8 HLindirect reg)
    ixIndirectLoad  reg  idxOffset = LD (Reg8Reg8 reg (IXindirect idxOffset))
    ixIndirectStore reg  idxOffset = LD (Reg8Reg8 (IXindirect idxOffset) reg)
    iyIndirectLoad  reg  idxOffset = LD (Reg8Reg8 reg (IYindirect idxOffset))
    iyIndirectStore reg  idxOffset = LD (Reg8Reg8 (IYindirect idxOffset) reg)

    testIndirectLoads (dstReg, dstSetter, _dstGetter, _dstVal, dstText) loadFunc memAddr idxOffset bannerStr =
      compareRegs z80expected z80' (T.pack (printf bannerStr dstText))
      where
        hlval        = fst $ sysMRead (memAddr + signExtend idxOffset) z80indirectSys
        z80'         = z80instructionExecute (DecodedInsn 0x1005 (loadFunc dstReg idxOffset)) z80indirectSys
        z80expected  = z80indirectSys & processor . cpu . regs . dstSetter .~ hlval

    testIndirectStores (srcReg, _srcSetter, srcGetter, _srcVal, srcText) storeFunc memAddr idxOffset bannerStr =
      compareMem (memAddr + signExtend idxOffset) expectedVal z80' (T.pack (printf bannerStr srcText))
      where
        z80'         = z80instructionExecute (DecodedInsn 0x1005 (storeFunc srcReg idxOffset)) z80indirectSys
        expectedVal  = z80registers z80indirectSys ^. srcGetter

test_ldReg8Imm
  :: TestOptions
  -> Assertion
test_ldReg8Imm opts =
  do
    immloads <- sequence [testReg8Imm reg | reg <- ordinaryReg8]
    assertBool "Immediate loads failed." (and immloads)
  where
    z80indirectSys  = z80randMem opts
    testReg8Imm (srcReg, srcSetter, _srcGetter, srcVal, srcText) =
      compareRegs z80expected z80' (T.pack (printf "LD %s, 0x%02x" srcText srcVal))
      where
        z80'        = z80instructionExecute (DecodedInsn 0x1003 (LD (Reg8Imm srcReg srcVal))) z80indirectSys
        z80expected = z80indirectSys & processor . cpu . regs . srcSetter .~ srcVal

test_ldOtherIndirect
  :: TestOptions
  -> Assertion
test_ldOtherIndirect opts =
  do
    resultBC <- compareRegs bcloadExpected bcload "LD A, (BC)"
    resultDE <- compareRegs deloadExpected deload "LD A, (DE)"
    resultImm <- compareRegs immloadExpected immload "LD A, (731FH)"
    storeBC <- compareMem bcIndAddr accumVal bcstore "LD (BC), A"
    storeDE <- compareMem deIndAddr accumVal destore "LD (DE), A"
    storeImm <- compareMem immIndAddr accumVal immstore "LD (731FH), A"
    assertBool "BC/DE/imm indirect loads/stores failed."
               (resultBC && resultDE && resultImm && storeBC && storeDE && storeImm)
  where
    bcIndAddr  = 0x725f
    deIndAddr  = 0x7266
    immIndAddr = 0x731f
    accumVal   = 0x9d

    z80indirectSys = z80randMem opts
    z80testSys  = z80indirectSys & processor . cpu . regs .~
                      (z80registers z80indirectSys &
                          z80accum .~ accumVal
                        & z80breg  .~ fromIntegral ((bcIndAddr `shiftR` 8) .&. 0xff)
                        & z80creg  .~ fromIntegral (bcIndAddr .&. 0xff)
                        & z80dreg  .~ fromIntegral ((deIndAddr `shiftR` 8) .&. 0xff)
                        & z80ereg  .~ fromIntegral (deIndAddr .&. 0xff))
    bcload = z80instructionExecute (DecodedInsn 0x1005 (LD AccBCIndirect)) z80testSys
    bcLoadVal = fst $ sysMRead bcIndAddr z80testSys
    bcloadExpected = z80testSys & processor . cpu . regs . z80accum .~ bcLoadVal
    deload = z80instructionExecute (DecodedInsn 0x1008 (LD AccDEIndirect)) z80testSys
    deLoadVal = fst $ sysMRead deIndAddr z80testSys
    deloadExpected = z80testSys & processor . cpu . regs . z80accum .~ deLoadVal
    immload = z80instructionExecute (DecodedInsn 0x100a (LD (AccImm16Indirect (AbsAddr immIndAddr)))) z80testSys
    immLoadVal = fst $ sysMRead immIndAddr z80testSys
    immloadExpected = z80testSys & processor . cpu . regs . z80accum .~ immLoadVal

    bcstore = z80instructionExecute (DecodedInsn 0x100c (LD BCIndirectStore)) z80testSys
    destore = z80instructionExecute (DecodedInsn 0x100d (LD DEIndirectStore)) z80testSys
    immstore = z80instructionExecute (DecodedInsn 0x100e (LD (Imm16IndirectStore (AbsAddr immIndAddr)))) z80testSys

test_ldSpecials
  :: TestOptions
  -> Assertion
test_ldSpecials opts =
  assertBool "LD A, (I|R)/LD (I|R), A failed" (resultAI && resultIA && resultAR && resultRA)
  where
    z80testSys = z80randMem opts
    iload = z80instructionExecute (DecodedInsn 0x1000 (LD AccIReg)) z80testSys
    resultAI = z80registers iload ^. z80accum == z80registers z80testSys ^. z80ipage
    istore = z80instructionExecute (DecodedInsn 0x1001 (LD IRegAcc)) z80testSys
    resultIA = z80registers istore ^. z80ipage == z80registers z80testSys ^. z80accum
    rload = z80instructionExecute (DecodedInsn 0x1002 (LD AccRReg)) z80testSys
    resultAR = z80registers rload ^. z80accum == z80registers z80testSys ^. z80rreg
    rstore = z80instructionExecute (DecodedInsn 0x1003 (LD RRegAcc)) z80testSys
    resultRA = z80registers rstore ^. z80rreg == z80registers z80testSys ^. z80accum


test_ldReg16Imm
  :: TestOptions
  -> Assertion
test_ldReg16Imm _opts =
  do
    results <- sequence [test_reg16imm reg | reg <- ordinaryReg16]
    spResult <- compareRegs spImmExpected spImmGot (T.pack (printf "LD SP, 0x3c4b"))
    assertBool "16-bit immediate loads" (and results && spResult)
  where
    test_reg16imm (rpair, reg, _getLens, val, name) =
      compareRegs regExpected regGot (T.pack (printf "LD %s, 0x%04x" name val))
      where
        regGot = z80instructionExecute (DecodedInsn 0x1101 (LD (RPair16ImmLoad rpair (AbsAddr val)))) z80initialCPU
        regExpected = reg16set val reg z80initialCPU
    spImmGot = z80instructionExecute (DecodedInsn 0x1104 (LD (RPair16ImmLoad SP (AbsAddr 0x3c4b)))) z80initialCPU
    spImmExpected = z80initialCPU & processor . cpu . regs . z80sp .~ 0x3c4b

test_ldReg16IndLoad
  :: TestOptions
  -> Assertion
test_ldReg16IndLoad opts =
  do
    stdGen <- getStdGen
    let (offset, stdGen') = randomR (0, 1023) stdGen :: (Z80addr, StdGen)
        indirectAddr      = 0x7200 + offset
    setStdGen stdGen'
    results <- sequence [test_reg16indLoad reg indirectAddr | reg <- ordinaryReg16]
    let spExpected = expectedVal indirectAddr
        spGot      = spGotVal indirectAddr
        spResult   = spExpected == spGot
    when (spExpected /= spGot) $ do
      printf "LD SP, (0x%04x): Expected 0x%04x, got 0x%04x\n" indirectAddr spExpected spGot
    assertBool "16-bit indirect loads" (and results)
    assertBool "SP indirect load" spResult
  where
    z80testSys = z80randMem opts
    test_reg16indLoad (rpair, _reg, getLens, _val, name) indirectAddr =
      do
        when (expectedVal indirectAddr /= gotVal) $ do
          printf "LD %s, (0x%04x): Expected 0x%04x, got 0x%04x\n" name indirectAddr (expectedVal indirectAddr) gotVal
        return (expectedVal indirectAddr == gotVal)
      where
        indLoad = z80instructionExecute (DecodedInsn 0x10f0 (LD (RPIndirectLoad rpair (AbsAddr indirectAddr)))) z80testSys
        gotVal = getLens indLoad
    expectedVal indirectAddr = make16bit . fst $ sysMReadN indirectAddr 2 z80testSys
    spIndLoad indirectAddr = z80instructionExecute (DecodedInsn 0x10f3 (LD (RPIndirectLoad SP (AbsAddr indirectAddr)))) z80testSys
    spGotVal indirectAddr = (spIndLoad indirectAddr) ^. processor . cpu . regs . z80sp


test_ldReg16IndStore
  :: TestOptions
  -> Assertion
test_ldReg16IndStore opts =
  do
    stdGen <- getStdGen
    let (offset, stdGen') = randomR (0, 1023) stdGen :: (Z80addr, StdGen)
        indirectAddr = 0x7200 + offset
    setStdGen stdGen'
    results <- sequence [test_reg16indStore reg indirectAddr | reg <- ordinaryReg16]
    spResult <- compareMem16 indirectAddr (expectedVal indirectAddr) (spIndStore indirectAddr)
                             (T.pack (printf "LD (0x%04x), SP" indirectAddr))
    assertBool "16-bit indirect stores" (and results)
    assertBool "SP indirect store" spResult
  where
    z80testSys = z80randMem opts
    test_reg16indStore (rpair, _reg, _getLens, _val, name) indirectAddr =
      compareMem16 indirectAddr (expectedVal indirectAddr) indStore (T.pack (printf "LD (0x%04x), %s" indirectAddr name))
      where
        indStore = z80instructionExecute (DecodedInsn 0x10f2 (LD (RPIndirectStore rpair (AbsAddr indirectAddr)))) z80testSys
    expectedVal indirectAddr = make16bit . fst $ sysMReadN indirectAddr 2 z80testSys
    spIndStore indirectAddr = z80instructionExecute (DecodedInsn 0x10f5 (LD (RPIndirectStore SP (AbsAddr indirectAddr)))) z80testSys
