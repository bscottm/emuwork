{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | General data structures and type classes for emulated processors.
module Machine.EmulatedSystem where

import Data.Data
import Control.Lens
import qualified Data.Text as T

import Machine.MemorySystem
import Machine.ProgramCounter
import Machine.Utils

-- | 'EmulatedProcessor' encapsulates general information about an emulated machine.
data EmulatedProcessor procType addrType instructionSet where
  EmulatedProcessor ::
    { _procPrettyName :: String                 -- ^ Pretty name for the emulated processor
    , _internals      :: procType               -- ^ Processor-specific internal data.
    } -> EmulatedProcessor  procType addrType instructionSet

-- Emit Template Haskell hair for the lenses
makeLenses ''EmulatedProcessor

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

-- | Processor operations type class
class ProcessorOps insnSet addrType wordType where
  -- | Instruction decoder, for disassembly and execution
  idecode :: ProgramCounter addrType
          -- ^ Current program counter, from where instructions are fetched
          -> MemorySystem addrType wordType
          -- ^ The memory system
          -> (DecodedInsn insnSet addrType, MemorySystem addrType wordType)
          -- ^ The decoded instruction

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem procInternals addrType wordType instructionSet where
  EmulatedSystem ::
    { _processor  :: EmulatedProcessor procInternals addrType instructionSet
                  -- ^ System processor
    , _memory     :: MemorySystem addrType wordType
                     -- ^ System memory
    , _sysName    :: String
                  -- ^ The system's name, e.g. "Null/dummy processor"
    , _sysAliases :: [String]
                  -- ^ Names the system is known by.
    } -> EmulatedSystem procInternals addrType wordType instructionSet

-- Need to manually generate the lenses due to the constraint on EmulatedSystem

processor :: Lens' (EmulatedSystem procType addrType wordType insnSet) (EmulatedProcessor procType addrType insnSet)
processor f sys = (\proc -> sys { _processor = proc }) <$> f (_processor sys)

memory :: Lens' (EmulatedSystem procType addrType wordType insnSet) (MemorySystem addrType wordType)
memory f sys = (\msys -> sys { _memory = msys }) <$> f (_memory sys)

sysName :: Lens' (EmulatedSystem procType addrType wordType insnSet) String
sysName f sys = (\name -> sys { _sysName = name }) <$> f (_sysName sys)

sysAliases :: Lens' (EmulatedSystem procType addrType wordType insnSet) [String]
sysAliases f sys = (\aliases -> sys { _sysAliases = aliases }) <$> f (_sysAliases sys)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | A (symbolic|absolute) address
data SymAbsAddr addrType where
  AbsAddr :: addrType
          -> SymAbsAddr addrType
  SymAddr :: T.Text
          -> SymAbsAddr addrType
  deriving (Typeable, Data)

instance (ShowHex addrType) => Show (SymAbsAddr addrType) where
  show (AbsAddr addr)  = as0xHexS addr
  show (SymAddr label) = show label
