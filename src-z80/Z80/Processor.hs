{-# LANGUAGE TemplateHaskell #-}

-- | The Z80 processor emulation module.
module Z80.Processor
  ( -- * Types
    Z80word
  , Z80addr
  , Z80disp
  , Z80ioPort
  , Z80registers(..)
  , Z80state(..)
  , Z80PC

    -- * Functions
  , z80MinAddr
  , z80MaxAddr
  , z80initialState
  , z80MemSizeIntegral

    -- * Lens functions
  , regs
  , z80pc
  , primes
  , iff1
  , iff2
  , intmode
  , flagSign
  , flagZero
  , flagYFlag
  , flagHalfCarry
  , flagXFlag
  , flagParOv
  , flagNFlag
  , flagCarry
  , z80accum
  , z80breg
  , z80creg
  , z80dreg
  , z80ereg
  , z80hreg
  , z80lreg
  , z80ixh
  , z80ixl
  , z80iyh
  , z80iyl
  , z80sp
  , z80ipage
  , z80rreg
  ) where

import           Lens.Micro.Platform (makeLenses)
import           Data.Int
import           Data.Word

import Machine   (ProgramCounter)

-- | Basic word type on a Z80: byte
type Z80word          = Word8
-- | Memory addresses are 16-bit quantities
type Z80addr          = Word16
-- | Program counter displacements are 16 bits
type Z80disp          = Int16
-- | Z80 program counter
type Z80PC            = ProgramCounter Z80addr
-- | Z80 I/O ports are 8-bit
type Z80ioPort        = Word8
--
-- | The minimum usable address
z80MinAddr :: Z80addr
z80MinAddr = minBound

-- | The maximum usable address
z80MaxAddr :: Z80addr
z80MaxAddr = maxBound

-- | The Z80's machine state and internals.
data Z80state =
        Z80state
        { _regs          :: Z80registers                      -- ^ Current operating register file
        , _primes        :: Z80registers                      -- ^ The "prime" (alternate) register set
        , _z80pc         :: Z80PC                             -- ^ Program counter
        , _iff1          :: Bool                              -- ^ Interrupt flip-flop 1
        , _iff2          :: Bool                              -- ^ Interrupt flip-flip 2
        , _intmode       :: Int                               -- ^ Interrupt mode (IM 0, 1 or 2)
        , _flagSign      :: Bool                              -- ^ Sign flag
        , _flagZero      :: Bool                              -- ^ Zero flag
        , _flagYFlag     :: Bool                              -- ^ "Y" flag: Bit 5 of the result (BCD adjustments)
        , _flagHalfCarry :: Bool                              -- ^ Half-carry: Carry to/from bit 4 to bit 3, BCD adjustments
        , _flagXFlag     :: Bool                              -- ^ "X" flag: Bit 3 of the result (BCD adjustments)
        , _flagParOv     :: Bool                              -- ^ Parity or overflow flag (operation dependent)
        , _flagNFlag     :: Bool                              -- ^ Add/subtract flag (BCD adjustments)
        , _flagCarry     :: Bool                              -- ^ Carry flag
        }
        deriving (Eq, Show)

-- | The basic Z80 register file. The actual register file has two sides, the regular and prime. The prime
-- registers are not generally visible except through the EXX instruction that exchanges the two sides.
data Z80registers = Z80registers {
  -- The individual registers. The register pairs are treated separately.
    _z80accum :: {-# UNPACK #-} !Z80word         -- ^ Accumulator
  , _z80breg  :: {-# UNPACK #-} !Z80word         -- ^ B register
  , _z80creg  :: {-# UNPACK #-} !Z80word         -- ^ C register
  , _z80dreg  :: {-# UNPACK #-} !Z80word         -- ^ D register
  , _z80ereg  :: {-# UNPACK #-} !Z80word         -- ^ E register
  , _z80hreg  :: {-# UNPACK #-} !Z80word         -- ^ "High" register
  , _z80lreg  :: {-# UNPACK #-} !Z80word         -- ^ "Low" register
  , _z80ixh   :: {-# UNPACK #-} !Z80word         -- ^ Upper 8 bits of IX
  , _z80ixl   :: {-# UNPACK #-} !Z80word         -- ^ Lower 8 bits of IX
  , _z80iyh   :: {-# UNPACK #-} !Z80word         -- ^ Upper 8 bits of IY
  , _z80iyl   :: {-# UNPACK #-} !Z80word         -- ^ Lower 8 bits of IY
  , _z80sp    :: {-# UNPACK #-} !Z80addr         -- ^ Stack pointer
  , _z80ipage :: {-# UNPACK #-} !Z80word         -- ^ Interrupt page
  , _z80rreg  :: {-# UNPACK #-} !Z80word         -- ^ (Dynamic memory) Refresh register
  }
  deriving (Eq, Show)

-- | Default/zeroed register set
initialRegisters :: Z80registers
initialRegisters = Z80registers {
    _z80accum = 0xff
  , _z80breg  = 0
  , _z80creg  = 0
  , _z80dreg  = 0
  , _z80ereg  = 0
  , _z80hreg  = 0
  , _z80lreg  = 0
  , _z80ixh   = 0
  , _z80ixl   = 0
  , _z80iyh   = 0
  , _z80iyl   = 0
  , _z80sp    = 0xffff
  , _z80ipage = 0
  , _z80rreg  = 0
  }

-- Emit Template Haskell hair for lenses
$(concat <$> mapM makeLenses [''Z80state, ''Z80registers])

-- | The address range, needed to calculate the size of a 'Data.Vector' data type
z80MemSizeIntegral :: Int
z80MemSizeIntegral = fromIntegral z80MaxAddr - fromIntegral z80MinAddr + 1

-- | Initial state for a Z80
z80initialState :: Z80state
z80initialState = Z80state
                  { _regs          = initialRegisters
                  , _primes        = initialRegisters
                  , _z80pc         = 0
                  , _iff1          = False
                  , _iff2          = False
                  , _intmode       = 0
                  , _flagSign      = True
                  , _flagZero      = True
                  , _flagYFlag     = True
                  , _flagHalfCarry = True
                  , _flagXFlag     = True
                  , _flagParOv     = True
                  , _flagNFlag     = True
                  , _flagCarry     = True
                   }
