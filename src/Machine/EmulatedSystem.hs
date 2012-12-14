-- | General data structures and type classes for emulated processors.
module Machine.EmulatedSystem where

import Control.Lens
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Text as T

import Machine.Utils

-- | 'EmulatedProcessor' encapsulates general information about an emulated machine.
data EmulatedProcessor procInternals =
  EmulatedProcessor
  { _procPrettyName :: String                    -- ^ Pretty name for the emulated processor
  , _procAliases    :: [String]                  -- ^ Other names by which this processor is known
  , _internals      :: procInternals             -- ^ Processor-specific internal data.
  }

-- Emit Template Haskell hair for the lenses
makeLenses ''EmulatedProcessor

-- | Generic program counter
data ProgramCounter addrType where
  PC :: ( Integral addrType 
        ) =>
        addrType
     -> ProgramCounter addrType

-- | Make program counters behave like numeric types
instance (Integral addrType) => Num (ProgramCounter addrType) where
  (PC a) + (PC b) = PC (a + b)
  (PC a) - (PC b) = PC (a - b)
  (PC a) * (PC b) = PC (a * b)
  abs (PC a)      = PC . abs $ a
  signum (PC a)   = PC . signum $ a
  fromInteger a   = PC (fromInteger a)

-- | Make program counters comparable
instance Ord (ProgramCounter addrType) where
  compare (PC a) (PC b) = compare a b

-- | Provide equality for program counters
instance Eq (ProgramCounter addrType) where
  (PC a) == (PC b) = a == b

-- | Make program counters show-able as something coherent.
instance (ShowHex addrType) => Show (ProgramCounter addrType) where
  show (PC pc) = "PC " ++ (T.unpack . as0xHex $ pc)

-- | Relative program counter data; 'dispType' should be a signed type of the same size as 'ProgramCounter's 'addrType'
data RelativePC dispType where
  RelativePC :: ( Integral dispType
                , SignExtend dispType
                ) =>
                dispType
             -> RelativePC dispType

instance (Integral dispType, SignExtend dispType) => Num (RelativePC dispType) where
  (RelativePC a) + (RelativePC b) = RelativePC (a + b)
  (RelativePC a) - (RelativePC b) = RelativePC (a - b)
  (RelativePC a) * (RelativePC b) = RelativePC (a * b)
  abs (RelativePC a)              = RelativePC . abs $ a
  signum (RelativePC a)           = RelativePC . signum $ a
  fromInteger a                   = RelativePC (fromInteger a)

instance Ord (RelativePC dispType) where
  compare (RelativePC a) (RelativePC b) = compare a b

instance Eq (RelativePC dispType) where
  (RelativePC a) == (RelativePC b) = a == b

-- | Basic program counter type class: increment, decrement, and displace
class GenericPC pcThing where
  -- | Increment the program counter
  pcInc      :: pcThing
             -> pcThing
  -- | Decrement the program counter
  pcDec      :: pcThing
             -> pcThing
  -- | Displace the program counter by a displacement amount (positive or negative).
  pcDisplace :: RelativePC dispType
             -> pcThing
             -> pcThing

-- | Memory system interface type.
--
-- The record type implementation is much more flexible than using type classes, since it cuts down on
-- the amount of type context/constraint that has to be provided.
data (Unbox wordType) => MemorySystem addrType wordType memInternals =
  MemorySystem
  { -- | The internal implementation of the memory system. For simple processors, like the Z80, this can be just
    -- a vector of bytes. More sophisticated processors can have more complicated implementations, obviously. The
    -- actual implementation depends on the system implemented.
    _memInternals :: memInternals
    -- | Fetch a word from memory.
  , _mfetch       :: (addrType -> wordType)
    -- | Fetch a block of words from memory.
  , _mfetchN      :: (addrType -> Int -> Vector wordType)
    -- | Query the highest address in the memory system (for unboxed vectors, this should be a synonym with
    -- 'Data.Vector.Unboxed.length')
  , _maxmem       :: addrType
  }

makeLenses ''MemorySystem

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem procInternals memInternals addrType wordType =
  EmulatedSystem
  { _processor :: EmulatedProcessor procInternals
  , _memory    :: MemorySystem addrType wordType memInternals
  }

makeLenses ''EmulatedSystem

-- | Emulator command line interface type class. This separates out the handling from the 'EmulatedProcessor'
-- processor internals, reducing the amount of polymorphic magic.
class EmuCommandLineDispatch procInternals where
  cmdDispatch    :: EmulatedProcessor procInternals
                 -> [String]
                 -> IO ()

-- !! FIXME !! this should really identify a system, not a processor.
-- | Identify this emulated processor by matching the requsted processor name to the processor's name and aliases
procIdentify :: EmulatedProcessor procInternals         -- ^ The emulated system
             -> String                                  -- ^ The emulator name
             -> Bool                                    -- ^ 'True' if matched.
procIdentify theProc name = theProc ^.  procAliases ^& (name `elem`)

-- | Get a program counter's actual (internal) value
getPCvalue :: ProgramCounter addrType
           -> addrType
getPCvalue (PC pc) = pc

-- | Fetch from memory and increment a program counter
memFetchAndIncPC :: (Unbox wordType) => 
                    MemorySystem addrType wordType memSys       -- ^ System memory interface
                 -> ProgramCounter addrType                     -- ^ Current Z80 state
                 -> (ProgramCounter addrType, wordType)         -- ^ New Z80 state and fetched byte
memFetchAndIncPC mem pc = let mFetchF = mem ^. mfetch
                              thePC   = getPCvalue pc
                          in  (pcInc pc, mFetchF thePC)

-- | Fetch from memory and increment a program counter
memIncPCAndFetch :: (Unbox wordType) => 
                    MemorySystem addrType wordType memSys       -- ^ System memory interface
                 -> ProgramCounter addrType                     -- ^ Current Z80 state
                 -> (ProgramCounter addrType, wordType)         -- ^ New Z80 state and fetched byte
memIncPCAndFetch mem pc = let mFetchF = mem ^. mfetch
                              newpc   = pcInc pc
                              thePC   = getPCvalue newpc
                          in  (newpc, mFetchF thePC)

-- | Instantiate 'GenericPC' operations for 'ProgramCounter'
instance GenericPC (ProgramCounter addrType) where
  pcInc (PC pc)                        = PC (pc + 1)
  pcDec (PC pc)                        = PC (pc - 1)
  pcDisplace (RelativePC disp) (PC pc) = PC (fromIntegral pc + fromIntegral disp)
