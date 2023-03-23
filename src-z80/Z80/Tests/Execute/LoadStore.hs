module Z80.Tests.Execute.LoadStore where

import           Data.Bits
import           Data.Text as T
import           Lens.Micro.Platform
import           System.Random           (StdGen, getStdGen, randomR, setStdGen)
import           Test.HUnit              (Assertion, assertBool)
import           Text.Printf

import           Machine
import           Z80

import           Z80.Tests.Execute.TestData
import           Z80.Tests.Execute.Utils

-- | Test the 8-bit-to-8-bit loads (LD A, B; LD B, (HL); LD H, (IX+3) ...)
test_ldReg8Reg8
  :: TestParams
  -> Assertion
test_ldReg8Reg8 opts =
  do
    reg8direct <- sequence [testReg8Load dst src | dst <- reg8TestData, src <- reg8TestData]
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
        hlLoads  <- sequence [testIndirectLoads  dst hlIndirectLoad  initialHLAddr 0x00       "LD %s, (HL)"
                             | dst <- reg8TestData]
        hlStores <- sequence [testIndirectStores src hlIndirectStore initialHLAddr 0x00       "LD (HL), %s"                           
                             | src <- reg8TestData]
        ixLoads  <- sequence [testIndirectLoads  dst ixIndirectLoad initialIXAddr ixOffset
                                                 (printf "LD %%s, (IX+0x%02x) [addr = 0x%04x]"
                                                         ixOffset
                                                         (initialIXAddr + signExtend ixOffset))
                             | dst <- reg8TestData]
        ixStores <- sequence [testIndirectStores src ixIndirectStore initialIXAddr ixOffset
                                                 (printf "LD (IX+0x%02x), %%s [addr = 0x%04x]"
                                                         ixOffset (initialIXAddr + signExtend ixOffset))
                             | src <- reg8TestData]
        iyLoads  <- sequence [testIndirectLoads  dst iyIndirectLoad initialIYAddr iyOffset
                                                 (printf "LD %%s, (IY+0x%02x) [addr = 0x%04x]"
                                                         iyOffset (initialIYAddr + signExtend iyOffset))
                             | dst <- reg8TestData]
        iyStores <- sequence [testIndirectStores src iyIndirectStore initialIYAddr iyOffset
                                                 (printf "LD (IY+0x%02x), %%s [addr = 0x%04x]"
                                                         iyOffset (initialIYAddr + signExtend iyOffset))
                             | src <- reg8TestData]
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
  :: TestParams
  -> Assertion
test_ldReg8Imm opts =
  do
    immloads <- sequence [testReg8Imm reg | reg <- reg8TestData]
    assertBool "Immediate loads failed." (and immloads)
  where
    z80indirectSys  = z80randMem opts
    testReg8Imm (srcReg, srcSetter, _srcGetter, srcVal, srcText) =
      compareRegs z80expected z80' (T.pack (printf "LD %s, 0x%02x" srcText srcVal))
      where
        z80'        = z80instructionExecute (DecodedInsn 0x1003 (LD (Reg8Imm srcReg srcVal))) z80indirectSys
        z80expected = z80indirectSys & processor . cpu . regs . srcSetter .~ srcVal

test_ldOtherIndirect
  :: TestParams
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
  :: TestParams
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
  :: TestParams
  -> Assertion
test_ldReg16Imm _opts =
  do
    results <- sequence [test_reg16imm reg | reg <- reg16TestData]
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
  :: TestParams
  -> Assertion
test_ldReg16IndLoad opts =
  do
    stdGen <- getStdGen
    let (offset, stdGen') = randomR (0, 1023) stdGen :: (Z80addr, StdGen)
        indirectAddr      = 0x7200 + offset
    setStdGen stdGen'
    results <- sequence [test_reg16indLoad reg indirectAddr | reg <- reg16TestData]
    let spExpected = expectedVal indirectAddr
        spGot      = spGotVal indirectAddr
        spResult   = spExpected == spGot
    when (spExpected /= spGot) $
      printf "LD SP, (0x%04x): Expected 0x%04x, got 0x%04x\n" indirectAddr spExpected spGot
    assertBool "16-bit indirect loads" (and results)
    assertBool "SP indirect load" spResult
  where
    z80testSys = z80randMem opts
    test_reg16indLoad (rpair, _reg, getLens, _val, name) indirectAddr =
      do
        when (expectedVal indirectAddr /= gotVal) $
          printf "LD %s, (0x%04x): Expected 0x%04x, got 0x%04x\n" name indirectAddr (expectedVal indirectAddr) gotVal
        return (expectedVal indirectAddr == gotVal)
      where
        indLoad = z80instructionExecute (DecodedInsn 0x10f0 (LD (RPIndirectLoad rpair (AbsAddr indirectAddr)))) z80testSys
        gotVal = getLens indLoad
    expectedVal indirectAddr = make16bit . fst $ sysMReadN indirectAddr 2 z80testSys
    spIndLoad indirectAddr = z80instructionExecute (DecodedInsn 0x10f3 (LD (RPIndirectLoad SP (AbsAddr indirectAddr)))) z80testSys
    spGotVal indirectAddr = spIndLoad indirectAddr ^. processor . cpu . regs . z80sp


test_ldReg16IndStore
  :: TestParams
  -> Assertion
test_ldReg16IndStore opts =
  do
    stdGen <- getStdGen
    let (offset, stdGen') = randomR (0, 1023) stdGen :: (Z80addr, StdGen)
        indirectAddr = 0x7200 + offset
    setStdGen stdGen'
    results <- sequence [test_reg16indStore reg indirectAddr | reg <- reg16TestData]
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
