-- | The Z80 processor emulation module.
module Z80.Processor
  ( -- * Types
    Z80word
  , Z80addr
  , Z80disp
  , Z80registers(..)
  , Z80state(..)
  , Z80PC
  , Z80memory

    -- * Functions
  , z80MinAddr
  , z80MaxAddr
  , z80initialState
  , z80MemSizeIntegral

    -- * Lens functions
  , regs
  , z80pc
  , primes
  , ix
  , iy
  , sp
  , ipage
  , refresh
  , iff1
  , iff2
  , intmode
  ) where

import Control.Lens (makeLenses)
import Data.Int
import Data.Word

import Machine

-- | Basic word type on a Z80: byte
type Z80word          = Word8
-- | Memory addresses are 16-bit quantities
type Z80addr          = Word16
-- | Program counter displacements are 16 bits
type Z80disp          = Int16
-- | Z80 program counter
type Z80PC            = ProgramCounter Z80addr
-- | Z80 memory system type
type Z80memory        = MemorySystem Z80addr Z80word

-- | The basic Z80 register file. The actual register file has two sides, the regular and prime. The prime
-- registers are not generally visible except through the EXX instruction that exchanges the two sides.
data Z80registers = Z80registers {
  -- The individual registers. The register pairs are treated separately.
    _z80accum :: {-# UNPACK #-} !Z80word         -- ^ Accumulator
  , _z80flags :: {-# UNPACK #-} !Z80word         -- ^ Flags
  , _z80breg  :: {-# UNPACK #-} !Z80word         -- ^ B register
  , _z80creg  :: {-# UNPACK #-} !Z80word         -- ^ C register
  , _z80dreg  :: {-# UNPACK #-} !Z80word         -- ^ D register
  , _z80ereg  :: {-# UNPACK #-} !Z80word         -- ^ E register
  , _z80hreg  :: {-# UNPACK #-} !Z80word         -- ^ "High" register
  , _z80lreg  :: {-# UNPACK #-} !Z80word         -- ^ "Low" register
  }

-- | Default/zeroed register set
initialRegisters :: Z80registers
initialRegisters = Z80registers {
    _z80accum = 0xff
  , _z80flags = 0xff
  , _z80breg = 0
  , _z80creg = 0
  , _z80dreg = 0
  , _z80ereg = 0
  , _z80hreg = 0
  , _z80lreg = 0
  }

-- | The minimum usable address
z80MinAddr :: Z80addr
z80MinAddr = minBound

-- | The maximum usable address
z80MaxAddr :: Z80addr
z80MaxAddr = maxBound

-- | The Z80's machine state and internals.
data Z80state =
        Z80state
        { _regs      :: Z80registers                            -- ^ Current operating register file
        , _z80pc     :: Z80PC                                   -- ^ Program counter
        , _primes    :: Z80registers                            -- ^ The "prime" register set
        , _ix        :: Z80addr                                 -- ^ IX index register
        , _iy        :: Z80addr                                 -- ^ IY index register
        , _sp        :: Z80addr                                 -- ^ Stack pointer
        , _ipage     :: Z80word                                 -- ^ Interrupt page
        , _refresh   :: Z80word                                 -- ^ (Dynamic memory) Refresh register
        , _iff1      :: Bool                                    -- ^ Interrupt flip-flop 1
        , _iff2      :: Bool                                    -- ^ Interrupt flip-flip 2
        , _intmode   :: Int                                     -- ^ Interrupt mode (IM 0, 1 or 2)
        }

-- Emit Template Haskell hair for lenses
$(makeLenses ''Z80state)

-- | The address range, needed to calculate the size of a 'Data.Vector' data type
z80MemSizeIntegral :: Int
z80MemSizeIntegral = fromIntegral z80MaxAddr - fromIntegral z80MinAddr + 1

-- | Initial state for a Z80
z80initialState :: Z80state
z80initialState = Z80state
                  { _regs    = initialRegisters
                  , _primes  = initialRegisters
                  , _z80pc   = 0
                  , _ix      = 0
                  , _iy      = 0
                  , _sp      = 0xffff
                  , _ipage   = 0
                  , _refresh = 0
                  , _iff1    = False
                  , _iff2    = False
                  , _intmode = 0
                  }