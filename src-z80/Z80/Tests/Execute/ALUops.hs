module Z80.Tests.Execute.ALUops where

import Data.Bits
import Data.Word
import           Lens.Micro.Platform

import Machine
import Z80

import Z80.Tests.Execute.TestData
import Z80.Tests.Execute.QuickInstances()
import Z80.Tests.Execute.Utils

-- import Debug.Trace

prop_subReg8
  :: (Word8, Word8, Z80reg8)
  -> Bool
prop_subReg8 inp@(_, _, A)       = accumResult inp SUB == 0
prop_subReg8 inp@(accum, val, _) = accumResult inp SUB == accum - val


prop_andReg8
  :: (Word8, Word8, Z80reg8)
  -> Bool
prop_andReg8 inp@(_, val, A)     = accumResult inp AND == val
prop_andReg8 inp@(accum, val, _) = accumResult inp AND == accum .&. val


prop_xorReg8
  :: (Word8, Word8, Z80reg8)
  -> Bool
prop_xorReg8 inp@(_, _, A)       = accumResult inp XOR == 0
prop_xorReg8 inp@(accum, val, _) = accumResult inp XOR == accum `xor` val


prop_orReg8
  :: (Word8, Word8, Z80reg8)
  -> Bool
prop_orReg8 inp@(_, val, A)     = accumResult inp OR == val
prop_orReg8 inp@(accum, val, _) = accumResult inp OR == accum .|. val


accumResult
  :: (Z80word, Z80word, Z80reg8)
  -> (OperALU -> Z80instruction)
  -> Z80word
accumResult (accum, val, reg) ins = z80registers testSys ^. z80accum
  where
    initialSys  = z80system & processor . cpu . regs .~
                   ( z80registers z80system
                     & z80accum .~ accum
                     & (z80Reg8Lens reg) .~ val
                   )
    testSys     = z80instructionExecute (DecodedInsn 0x1000 (ins (ALUreg8 reg))) initialSys
