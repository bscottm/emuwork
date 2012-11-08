{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

import Machine

type Z80word = Word8            -- ^ Basic word type on a Z80: byte
type Z80addr = Word16           -- ^ Memory addresses are 16-bit quantities
type Z80disp = Int16            -- ^ Program counter displacements are 16 bits

-- | The Z80's machine state and internals.
data Z80state =
        Z80state
        { regs      :: Z80registers                              -- ^ Current operating register file
        , primes    :: Z80registers                              -- ^ The "prime" register set
        , ix        :: Z80addr                                   -- ^ IX index register
        , iy        :: Z80addr                                   -- ^ IY index register
        , sp        :: Z80addr                                   -- ^ Stack pointer
        , ipage     :: Z80word                                   -- ^ Interrupt page
        , refresh   :: Z80word                                   -- ^ (Dynamic memory) Refresh register
        , intEnable :: Bool                                      -- ^ Interrupt enable flag
        , memory    :: Vector Z80word                            -- ^ Processor memory
        }
        
instance EmulatorActions Z80word Z80addr Z80disp Z80state where
  programCounter = z80programCounter
  -- stepOne = undefined
           
-- | The Z80's program counter manipulation function
z80programCounter :: ProgramCounterF Z80addr Z80disp
z80programCounter addr Inc      = addr + 1
z80programCounter addr Dec      = addr - 1
z80programCounter addr (Disp x) = addr + (fromIntegral x)
z80programCounter _    (Abs x)  = x

{- |
   The basic Z80 register file. The actual register file has two sides,
   the regular and prime. The prime registers are not generally visible
   except through the EXX instruction that exchanges the two sides.
-}
data Z80registers = Z80registers {
  -- The individual registers. The register pairs are treated separately.
    a :: Z80word                -- ^ Accumulator
  , f :: Z80word                -- ^ Flags
  , b :: Z80word                -- ^ B register
  , c :: Z80word                -- ^ C register
  , d :: Z80word                -- ^ D register
  , e :: Z80word                -- ^ E register
  , h :: Z80word                -- ^ "High" register
  , l :: Z80word                -- ^ "Low" register
  }
                    
-- | Default/zeroed register set
zeroedRegisters :: Z80registers
zeroedRegisters = Z80registers {
    a = 0
  , f = 0
  , b = 0
  , c = 0
  , d = 0
  , e = 0
  , h = 0
  , l = 0
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
  { regs = initialRegs
  , primes = zeroedRegisters
  , ix = 0
  , iy = 0
  , sp = 0
  , ipage = 0
  , refresh = 0
  , intEnable = False
  , memory = replicate z80MemSizeIntegral (0 :: Z80word)
  }

-- | Initial state for a Z80
z80initialState :: Z80state
z80initialState = initialState zeroedRegisters
