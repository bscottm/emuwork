{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{- | Machine emulation data types.
-}

module Machine.System where

import           Control.Lens
import           Control.Arrow                 (second)
import           Data.Data
import qualified Data.Text                     as T
import           Data.Vector.Unboxed           (Vector)
import qualified Data.Vector.Unboxed           as DVU

import           Machine.ProgramCounter
import           Machine.Utils
import qualified Machine.MemorySystem          as M

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem cpuType insnSet addrType wordType where
  EmulatedSystem ::
    { _processor  :: EmulatedProcessor cpuType insnSet addrType wordType
                  -- ^ System processor
    , _memory     :: M.MemorySystem addrType wordType
                     -- ^ System memory
    , _sysName    :: String
                  -- ^ The system's name, e.g. "Null/dummy processor"
    , _sysAliases :: [String]
                  -- ^ Names the system is known by.
    } -> EmulatedSystem cpuType insnSet addrType wordType
    deriving (Show)

-- Need to manually generate the lenses due to the constraint on EmulatedSystem

processor :: Lens' (EmulatedSystem cpuType insnSet addrType wordType) (EmulatedProcessor cpuType insnSet addrType wordType)
processor f sys = (\proc -> sys { _processor = proc }) <$> f (_processor sys)

memory :: Lens' (EmulatedSystem cpuType insnSet addrType wordType) (M.MemorySystem addrType wordType)
memory f sys = (\msys -> sys { _memory = msys }) <$> f (_memory sys)

sysName :: Lens' (EmulatedSystem cpuType addrType wordType insnSet) String
sysName f sys = (\name -> sys { _sysName = name }) <$> f (_sysName sys)

sysAliases :: Lens' (EmulatedSystem cpuType addrType wordType insnSet) [String]
sysAliases f sys = (\aliases -> sys { _sysAliases = aliases }) <$> f (_sysAliases sys)

-- | Emulated system constructor.
mkEmulatedSystem :: EmulatedProcessor cpuType insnSet addrType wordType
                 -- ^ Processor (CPU)
                 -> String
                 -- ^ System name
                 -> [String]
                 -- ^ System aliases
                 -> EmulatedSystem cpuType insnSet addrType wordType
mkEmulatedSystem sysCpu name aliases = EmulatedSystem { _processor = sysCpu
                                                      , _memory = M.initialMemorySystem
                                                      , _sysName = name
                                                      , _sysAliases = aliases
                                                      }

-- | 'EmulatedProcessor' boxes the emulated machine's processor. All interesting operations on an @EmulatedProcessor@
-- are in the `ProcessorOps` type class.
data EmulatedProcessor cpuType insnSet addrType wordType where
  EmulatedProcessor ::
    { _procPrettyName :: String
    -- ^ Pretty name for the emulated processor
    , _cpu            :: cpuType
    -- ^ Processor-specific internal data (registers, flags, etc.)
    , _ops            :: ProcessorOps cpuType insnSet addrType wordType
    -- ^ Processor operation functions
    } -> EmulatedProcessor  cpuType insnSet addrType wordType
    deriving (Show)

-- | Lens for the processor's pretty name
procPrettyName :: Lens' (EmulatedProcessor cpuType insnSet addrType wordType) String
procPrettyName f proc = (\name' -> proc { _procPrettyName = name' }) <$> f (_procPrettyName proc)

-- | Lens for the processor's internals (the CPU).
cpu :: Lens' (EmulatedProcessor cpuType insnSet addrType wordType) cpuType
cpu f proc = proc <$ f (_cpu proc)

processorOps :: Lens' (EmulatedProcessor cpuType insnSet addrType wordType) (ProcessorOps cpuType insnSet addrType wordType)
processorOps f ops =  ops <$ f (_ops ops)

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
data ProcessorOps cpuType insnSet addrType wordType where
  ProcessorOps ::
    {
      -- | Instruction decoder, for disassembly and execution
      _idecode :: ProcessorDecoder cpuType insnSet addrType wordType
    } -> ProcessorOps cpuType insnSet addrType wordType

idecode :: Lens' (ProcessorOps cpuType insnSet addrType wordType) (ProcessorDecoder cpuType insnSet addrType wordType)
idecode f decoder = decoder <$ f (_idecode decoder)

instance Show (ProcessorOps cpuType insnSet addrType wordType) where
  -- Nothing to show. Need the instance for the EmulatedSystem automagically derived Show instance.
  show ProcessorOps{} = ""

type ProcessorDecoder cpuType insnSet addrType wordType =
  ProgramCounter addrType
  -- ^ Current program counter, from where instructions are fetched
  -> EmulatedSystem cpuType insnSet addrType wordType
  -- ^ The memory system
  -> (DecodedInsn insnSet addrType, EmulatedSystem cpuType insnSet addrType wordType)
  -- ^ The decoded instruction


-- | I/O system based on ports, e.g., Zilog and Intel, are an address space of their own, which makes
-- a type alias for `MemorySystem` appropriate. Port-based I/O systems use ports for their address space and this
-- results in a better code re-use.
type IOSystem portType wordType = M.MemorySystem portType wordType


-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Symbolic and absolute addresses, used both by disassemblers and instruction sets. For example, the Z80's instruction set uses
-- both symbolic and absolute addresses as jump and call targets.
data SymAbsAddr addrType where
  AbsAddr :: addrType
          -> SymAbsAddr addrType
  SymAddr :: T.Text
          -> SymAbsAddr addrType
  deriving (Typeable, Data)

instance (ShowHex addrType) => Show (SymAbsAddr addrType) where
  show (AbsAddr addr)  = as0xHexS addr
  show (SymAddr label) = show label

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- System program counter operations
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

sysReadAndIncPC :: ( Integral addrType
                   , Integral wordType
                   , DVU.Unbox wordType
                   )
                => ProgramCounter addrType
                -> EmulatedSystem cpuType insnSet addrType wordType
                -> (ProgramCounter addrType, (wordType, EmulatedSystem cpuType insnSet addrType wordType))
sysReadAndIncPC pc sys = (1+ pc, sysMRead (unPC pc) sys)

-- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
sysIncPCAndRead :: ( Integral addrType
                   , Integral wordType
                   , DVU.Unbox wordType
                   )
                => ProgramCounter addrType
                -> EmulatedSystem cpuType insnSet addrType wordType
                -> (ProgramCounter addrType, (wordType, EmulatedSystem cpuType insnSet addrType wordType))
sysIncPCAndRead pc sys = (pc', sysMRead (unPC pc') sys)
  where
    pc'               = 1+ pc


sysMRead :: ( Integral addrType
            , Integral wordType
            , DVU.Unbox wordType
            )
         => addrType
         -> EmulatedSystem cpuType insnSet addrType wordType
         -> (wordType, EmulatedSystem cpuType insnSet addrType wordType)
sysMRead addr sys = second updateMemSys $ views memory (M.mRead addr) sys
  where
    updateMemSys msys = sys & memory .~ msys

sysPCReadN :: ( Integral addrType
              , Integral wordType
              , DVU.Unbox wordType
              )
           => ProgramCounter addrType
           -> Int
           -> EmulatedSystem cpuType insnSet addrType wordType
           -> (ProgramCounter addrType, (Vector wordType, EmulatedSystem cpuType insnSet addrType wordType))
sysPCReadN pc nwords sys = (pc + fromIntegral nwords, sysMReadN (unPC pc) nwords sys)

sysMReadN :: ( Integral addrType
             , Integral wordType
             , DVU.Unbox wordType
             )
          => addrType
          -> Int
          -> EmulatedSystem cpuType insnSet addrType wordType
          -> (Vector wordType, EmulatedSystem cpuType insnSet addrType wordType)
sysMReadN addr nwords sys = second updateMemSys $ views memory (M.mReadN addr nwords) sys
  where
    updateMemSys msys = sys & memory .~ msys

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- The null processor and system:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Null CPU type. Very simple.
data NullCPU = NullCPU
  deriving (Show)

-- | Instruction set: Just a no-operation.
data NullInsnSet = NullNOP

type NullSystem addrType wordType = EmulatedSystem NullCPU NullInsnSet addrType wordType

-- | The null system:
nullSystem :: NullSystem addrType wordType
nullSystem = mkEmulatedSystem nullProcessor name aliases
  where
    name     = "Null/dummy system"
    aliases  = ["null", "dummy"]

-- | The null processor
nullProcessor :: EmulatedProcessor NullCPU NullInsnSet addrType wordType
nullProcessor = EmulatedProcessor { _procPrettyName = "Null (dummy) processor"
                                  , _cpu            = NullCPU
                                  , _ops            = ProcessorOps
                                                      {
                                                        _idecode = nullIDecode
                                                      }
                                  }

-- | Null processor operations
{- instance ProcessorOps NullCPU NullInsnSet addrType wordType where
  idecode pc sys = (DecodedInsn pc NullNOP, sys) -}

nullIDecode :: ProcessorDecoder NullCPU NullInsnSet addrType wordType
nullIDecode pc sys = (DecodedInsn pc NullNOP, sys)
