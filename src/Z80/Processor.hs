{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The Z80 processor emulation module.
module Z80.Processor (
  -- * Types
    Z80word
  , Z80addr
  , Z80disp
  , Z80registers(..)
  , Z80state(..)

  -- * Functions
  , z80initialState
) where

import Prelude hiding (replicate)
import Data.Word
import Data.Int
import Data.Vector.Unboxed (Vector, replicate)

-- import Machine

type Z80word = Word8            -- ^ Basic word type on a Z80: byte
type Z80addr = Word16           -- ^ Memory addresses are 16-bit quantities
type Z80disp = Int16            -- ^ Program counter displacements are 16 bits

-- | The Z80's machine state and internals.
data Z80state =
        Z80state
        { _regs      :: Z80registers                            -- ^ Current operating register file
        , _primes    :: Z80registers                            -- ^ The "prime" register set
        , _ix        :: Z80addr                                 -- ^ IX index register
        , _iy        :: Z80addr                                 -- ^ IY index register
        , _sp        :: Z80addr                                 -- ^ Stack pointer
        , _ipage     :: Z80word                                 -- ^ Interrupt page
        , _refresh   :: Z80word                                 -- ^ (Dynamic memory) Refresh register
        , _iff1      :: Bool                                    -- ^ Interrupt flip-flop 1
        , _iff2      :: Bool                                    -- ^ Interrupt flip-flip 2
        , _memory    :: Vector Z80word                          -- ^ Processor memory
        }
        
{- |
   The basic Z80 register file. The actual register file has two sides,
   the regular and prime. The prime registers are not generally visible
   except through the EXX instruction that exchanges the two sides.
-}
data Z80registers = Z80registers {
  -- The individual registers. The register pairs are treated separately.
    _z80accum :: Z80word         -- ^ Accumulator
  , _z80flags :: Z80word         -- ^ Flags
  , _z80breg :: Z80word          -- ^ B register
  , _z80creg :: Z80word          -- ^ C register
  , _z80dreg :: Z80word          -- ^ D register
  , _z80ereg :: Z80word          -- ^ E register
  , _z80hreg :: Z80word          -- ^ "High" register
  , _z80lreg :: Z80word          -- ^ "Low" register
  }
                    
-- | Default/zeroed register set
zeroedRegisters :: Z80registers
zeroedRegisters = Z80registers {
    _z80accum = 0
  , _z80flags = 0
  , _z80breg = 0
  , _z80creg = 0
  , _z80dreg = 0
  , _z80ereg = 0
  , _z80hreg = 0
  , _z80lreg = 0
  }
                  
-- | The minimum usable address
z80MinAddr :: Z80addr
z80MinAddr = 0

-- | The maximum usable address
z80MaxAddr :: Z80addr
z80MaxAddr = 0xffff

-- | The address range, needed to calculate the size of a 'Data.Vector' data type
z80MemSizeIntegral :: Int
z80MemSizeIntegral = (fromIntegral z80MaxAddr) - (fromIntegral z80MinAddr) + 1

-- | Create the initial state of the Z80 processor. This sets the register
-- file to a user-provided initial state and zeroes out all other registers.
--  Interrupts are disabled.
--
-- FIXME: Is this YAGNI?
initialState :: Z80registers    -- ^ Initial register values
             -> Z80state        -- ^ Resulting state
initialState initialRegs = 
  Z80state
  { _regs = initialRegs
  , _primes = zeroedRegisters
  , _ix = 0
  , _iy = 0
  , _sp = 0
  , _ipage = 0
  , _refresh = 0
  , _iff1 = False
  , _iff2 = False
  , _memory = replicate z80MemSizeIntegral (0 :: Z80word)
  }

-- | Initial state for a Z80
z80initialState :: Z80state
z80initialState = initialState zeroedRegisters
