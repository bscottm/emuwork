{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}

-- | General data structures and type classes for emulated processors.
module Machine.EmulatedSystem where

import Data.Data
import Control.Lens
import Data.Functor.Identity
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
  fromInteger a                   = RelativePC . fromInteger $ a

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

-- | Do an action on a program counter
withPC :: ProgramCounter addrType -> (addrType -> value) -> value
withPC (PC pc) f = f pc
{-# INLINE withPC #-}

-- | 'EmulatedProcessor' encapsulates general information about an emulated machine.
data EmulatedProcessor procInternals addrType instructionSet where
  EmulatedProcessor ::
    { _procPrettyName :: String                 -- ^ Pretty name for the emulated processor
    , _internals      :: procInternals          -- ^ Processor-specific internal data.
    } -> EmulatedProcessor  procInternals addrType instructionSet

-- Emit Template Haskell hair for the lenses
makeLenses ''EmulatedProcessor

-- | Type class for memory system operations.
class MemoryOps memSys addrType wordType where
  -- | Fetch a single entity from memory
  mFetch :: memSys
        -> addrType
        -> wordType

  -- | Fetch multiple entities from memory
  mFetchN :: memSys
         -> addrType
         -> Int
         -> Vector wordType

  -- | Fetch an entity from memory at the current program counter, return the (incremented pc, contents)
  -- pair.
  mFetchAndIncPC :: (GenericPC addrType) =>
                    ProgramCounter addrType
                    -- ^ The program counter
                 -> memSys
                 -- ^ The memory system
                 -> (ProgramCounter addrType, wordType)
  mFetchAndIncPC pc mem = (pcInc pc, withPC pc (mFetch mem))

  -- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
  mIncPCAndFetch :: (GenericPC addrType) =>
                    ProgramCounter addrType
                 -> memSys
                 -> (ProgramCounter addrType, wordType)
  mIncPCAndFetch pc mem = let pc' = pcInc pc
                          in  (pc', withPC pc' (mFetch mem))

-- | A memory system, for a given address type and word type.
data MemorySystem addrType wordType where
  MemorySystem :: MemoryOps memSysType addrType wordType =>
                  memSysType
               -> MemorySystem addrType wordType

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
type InsnDecodeF instructionSet addrType wordType =
  (    ProgramCounter addrType                          --  Current program counter
    -> MemorySystem addrType wordType                   --  Memory system from which entities are fetched
    -> DecodedInsn instructionSet addrType              --  Decoder result
  )

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem procInternals addrType wordType instructionSet where
  EmulatedSystem ::
    { _processor  :: EmulatedProcessor procInternals addrType instructionSet
                     -- ^ System processor
    , _memory     :: MemorySystem addrType wordType
                     -- ^ System memory
    , _idecode    :: InsnDecodeF instructionSet addrType wordType
                     -- ^ Instruction decoding function. Used to disassemble instructions as well
                     -- as execute them.
    , _sysName    :: String
                  -- ^ The system's name, e.g. "Null/dummy processor"
    , _sysAliases :: [String]
                  -- ^ System identity aliases, e.g., "null", "trs80-model-I" used to identify the
                  -- emulator.
    } -> EmulatedSystem procInternals addrType wordType instructionSet

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

-- | Instantiate 'GenericPC' operations for 'ProgramCounter'
instance GenericPC (ProgramCounter addrType) where
  pcInc (PC pc)                        = PC (pc + 1)
  pcDec (PC pc)                        = PC (pc - 1)
  pcDisplace (RelativePC disp) (PC pc) = PC (fromIntegral pc + fromIntegral disp)
