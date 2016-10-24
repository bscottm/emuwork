{-# LANGUAGE DeriveDataTypeable #-}

-- | General data structures and type classes for emulated processors.
module Machine.EmulatedSystem where

import Data.Data
import Control.Lens
import Data.Vector.Unboxed (Vector)
import qualified Data.Text as T

import Machine.Utils

-- | Generic program counter
data ProgramCounter addrType where
  PC :: (Integral addrType) => addrType
     -> ProgramCounter addrType

-- | Make program counters behave like numeric types
instance (Integral addrType) => Num (ProgramCounter addrType) where
  (PC a) + (PC b) = PC (a + b)
  (PC a) - (PC b) = PC (a - b)
  (PC a) * (PC b) = PC (a * b)
  abs (PC a)      = PC (abs a)
  signum (PC a)   = PC (signum a)
  fromInteger a   = PC (fromInteger a)

-- | Make program counters comparable
instance Ord (ProgramCounter addrType) where
  compare (PC a) (PC b) = compare a b

-- | Provide equality comparisons for program counters
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

-- | 'EmulatedProcessor' encapsulates general information about an emulated machine.
data EmulatedProcessor procInternals addrType instructionSet where
  EmulatedProcessor ::
    { _procPrettyName :: String                 -- ^ Pretty name for the emulated processor
    , _internals      :: procInternals          -- ^ Processor-specific internal data.
    } -> EmulatedProcessor  procInternals addrType instructionSet

-- Emit Template Haskell hair for the lenses
makeLenses ''EmulatedProcessor

-- | Type class for memory systems.
class MemorySystem memSys addrType wordType where
  fetch :: memSys
        -> addrType
        -> wordType

  fetchN :: memSys
         -> addrType
         -> Int
         -> Vector wordType

{-
  -- | Query the highest address in the memory system (for unboxed vectors, this should be a synonym with
  -- 'Data.Vector.Unboxed.length')
  maxMem :: memSys
         -> addrType
-}

-- | Generic representation of instruction decoder outputs
data DecodedInsn instructionSet addrType where
  -- A decoded instruction
  DecodedInsn :: ProgramCounter addrType
              -> instructionSet
              -> DecodedInsn instructionSet addrType
  -- An address fetched from memory, independent of endian-ness
  DecodedAddr :: ProgramCounter addrType
              -> addrType
              -> DecodedInsn instructionSet addrType

-- | Instruction decoder function signature shorthand
type InsnDecodeF instructionSet addrType wordType memSys =
  (    ProgramCounter addrType                          --  Current program counter
    -> memSys                                           --  Memory system from which words are fetched
    -> DecodedInsn instructionSet addrType              --  Decoder result
  )

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem procInternals memInternals addrType wordType instructionSet where
  EmulatedSystem ::
    { _processor  :: EmulatedProcessor procInternals addrType instructionSet
                     -- ^ System processor
    , _memory     :: (MemorySystem addrType wordType memSys) => memSys
                     -- ^ System memory
    , _idecode    :: InsnDecodeF instructionSet addrType wordType memInternals
                     -- ^ Instruction decoding function. Used to disassemble instructions as well
                     -- as execute them.
    , _sysName    :: String
                  -- ^ The system's name, e.g. "Null/dummy processor"
    , _sysAliases :: [String]
                  -- ^ System identity aliases, e.g., "null", "trs80-model-I" used to identify the
                  -- emulator.
    } -> EmulatedSystem procInternals memInternals addrType wordType instructionSet

makeLenses ''EmulatedSystem

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | A (symbolic|absolute) address
data SymAbsAddr addrType where
  AbsAddr :: addrType
          -> SymAbsAddr addrType
  SymAddr :: T.Text
          -> SymAbsAddr addrType
  deriving (Typeable, Data)

instance (ShowHex addrType) =>
         Show (SymAbsAddr addrType) where
  show (AbsAddr addr)  = as0xHexS addr
  show (SymAddr label) = show label

-- | Do an action on a program counter
withPC :: ProgramCounter addrType -> (addrType -> value) -> value
withPC (PC pc) f = f pc

getPCvalue :: ProgramCounter addrType
           -> addrType
getPCvalue (PC pc) = pc

-- | Fetch from memory at the current program counter's address, returning the incremented program counter and
-- the fetched value
memFetchAndIncPC :: (MemorySystem addrType wordType memSys) => memSys
                 -- ^ System memory interface
                 -> ProgramCounter addrType
                 -- ^ Current Z80 state
                 -> (ProgramCounter addrType, wordType)
                 -- ^ New PC and fetched byte
memFetchAndIncPC mem pc = (pcInc pc, withPC pc (fetch mem))

-- | Instantiate 'GenericPC' operations for 'ProgramCounter'
instance GenericPC (ProgramCounter addrType) where
  pcInc (PC pc)                        = PC (pc + 1)
  pcDec (PC pc)                        = PC (pc - 1)
  pcDisplace (RelativePC disp) (PC pc) = PC (fromIntegral pc + fromIntegral disp)
