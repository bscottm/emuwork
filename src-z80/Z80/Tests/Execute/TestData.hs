module Z80.Tests.Execute.TestData where

import Data.Bits
import Data.Text (Text)
import Lens.Micro.Platform

import Machine
import Z80

newtype TestParams =
  TestParams
    { z80randMem :: Z80system Z80BaseSystem
    }

z80system :: Z80system Z80BaseSystem
z80system = z80generic & sysName .~ "Test Z80 generic system"
                       & sysAliases .~ []
                       & memory .~ msysRAMRegion 0x7200 1024
                          -- IX indirect operations memory page (offsets are 8-bit signed)
                          <> msysRAMRegion (initialIXAddr - 128)  256
                          -- IY indirect operations memory page (-128 -> +127)
                          <> msysRAMRegion (initialIYAddr - 128)  256

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

-- | The ordinary 8-bit registers (does not include the indirect (HL), (IX|IY+d) memory references). The first tuple
-- argument is (obviously) the reigster. The second and third are 'Z80state' register setters and getters. You have to
-- have separate setters and getters, even though they are the same and combine the two operations, because the Haskell
-- type system will disallow using the same function in two different ways. The fourth argument is the test value.
reg8TestData
  :: [(Z80reg8
      , ASetter Z80registers Z80registers Z80word Z80word
      , Getting Z80word Z80registers Z80word
      , Z80word
      , Text
      )]
reg8TestData =
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
reg16TestData
  :: [(RegPairSP, Z80reg16, Z80system sysType -> Z80addr, Z80addr, Text)]
reg16TestData =
  [ (RPair16 BC, BC, reg16get BC, 0x1234, "BC")
  , (RPair16 DE, DE, reg16get DE, 0x5678, "DE")
  , (RPair16 HL, HL, reg16get HL, 0x9abc, "HL")
  , (RPair16 IX, IX, reg16get IX, 0xdef0, "IX")
  , (RPair16 IY, IY, reg16get IY, 0x1f2e, "IY")
  ]
