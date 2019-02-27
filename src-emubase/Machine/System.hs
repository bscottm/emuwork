{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{- | Machine emulation data types.
-}

module Machine.System where

import           Control.Lens
import           Data.Data
import qualified Data.Text                     as T
import           Data.Vector.Unboxed            ( Vector )
import qualified Data.Vector.Unboxed           as DVU
import           Text.Printf
import           Generics.SOP.TH                (deriveGeneric)

import           Machine.ProgramCounter
import           Machine.Utils
import qualified Machine.MemorySystem          as M

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem cpuType insnSet addrType wordType =
  EmulatedSystem
    { _processor  :: EmulatedProcessor cpuType insnSet addrType wordType
                  -- ^ System processor
    , _memory     :: M.MemorySystem addrType wordType
                     -- ^ System memory
    , _sysName    :: String
                  -- ^ The system's name, e.g. "Null/dummy processor"
    , _sysAliases :: [String]
                  -- ^ Names the system is known by.
    }
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
mkEmulatedSystem
  :: (Ord addrType, Bounded addrType)
  => EmulatedProcessor cpuType insnSet addrType wordType
  -- ^ Processor (CPU)
  -> String
  -- ^ System name
  -> [String]
  -- ^ System aliases
  -> EmulatedSystem cpuType insnSet addrType wordType
  -- ^ Constructed emulated system
mkEmulatedSystem sysCpu name aliases =
  EmulatedSystem { _processor = sysCpu, _memory = M.initialMemorySystem, _sysName = name, _sysAliases = aliases }

-- | 'EmulatedProcessor' boxes the emulated machine's processor. All interesting operations on an @EmulatedProcessor@
-- are in the `ProcessorOps` data type.
data EmulatedProcessor cpuType insnSet addrType wordType where
  EmulatedProcessor ::{ _procPrettyName :: String
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
processorOps f ops = ops <$ f (_ops ops)


-- | Generic representation of instruction decoder outputs. Each decoded instruction's program counter refers to the address
-- that follows the instruction, _not_ the address of the instruction.
data DecodedInsn instructionSet addrType =
  -- | A decoded instruction
  DecodedInsn
  { _newPC :: ProgramCounter addrType
  -- ^ Program counter following the instruction
  , _insn :: instructionSet
  -- ^ The instruction to execute
  }
  deriving (Eq, Data, Typeable)

-- | Lens for the decoded instruction's program counter
decodedInsnPC :: Lens' (DecodedInsn insnSet addrType) (ProgramCounter addrType)
decodedInsnPC f insn = (\pc' -> insn { _newPC = pc' }) <$> f (_newPC insn)
-- | Lens for the decoded instruction itself.
decodedInsn :: Lens' (DecodedInsn insnSet addrType) insnSet
decodedInsn f insn = (\insn' -> insn { _insn = insn' }) <$> f (_insn insn)


-- | Processor operations function catalog.
data ProcessorOps cpuType insnSet addrType wordType =
  ProcessorOps
  {
    -- | Instruction decoder, for disassembly and execution
    _idecode :: ProcessorDecoder cpuType insnSet addrType wordType
  }

-- | Lens for the instruction decoder function. Note that this is effectively "read-only" -- one cannot change the
-- instruction decoder function.
idecode :: Lens' (ProcessorOps cpuType insnSet addrType wordType) (ProcessorDecoder cpuType insnSet addrType wordType)
idecode f decoder = decoder <$ f (_idecode decoder)

instance Show (ProcessorOps cpuType insnSet addrType wordType) where
  -- Nothing to show. Need the instance for the EmulatedSystem automagically derived Show instance.
  show ProcessorOps{} = ""

-- | Instruction decoder function type.
type ProcessorDecoder cpuType insnSet addrType wordType
  =  ProgramCounter addrType
  -- ^ Current program counter, from where instructions are fetched
  -> EmulatedSystem cpuType insnSet addrType wordType
  -- ^ The emulated system (uses the system's memory to read instruction and operands.)
  -> (DecodedInsn insnSet addrType, EmulatedSystem cpuType insnSet addrType wordType)
  -- ^ The decoded instruction and updated emulated system (because system's memory can change/get updated.)


-- | I/O system based on ports, e.g., Zilog and Intel, are an address space of their own, which makes
-- a type alias for `MemorySystem` appropriate. Port-based I/O systems use ports for their address space and this
-- results in a better code re-use.
type IOSystem portType wordType = M.MemorySystem portType wordType


-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Symbolic and absolute addresses, used both by disassemblers and instruction sets. For example, the Z80's instruction set uses
-- both symbolic and absolute addresses as jump and call targets.
data SymAbsAddr addrType =
    AbsAddr addrType
  | SymAddr T.Text
  deriving (Eq, Ord, Typeable, Data)

instance (ShowHex addrType) => Show (SymAbsAddr addrType) where
  show (AbsAddr addr ) = as0xHexS addr
  show (SymAddr label) = show label

$(deriveGeneric ''SymAbsAddr)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- System program counter and memory reading operations
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

sysReadAndIncPC
  :: (Integral addrType, Integral wordType, DVU.Unbox wordType)
  => ProgramCounter addrType
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> (ProgramCounter addrType, (wordType, EmulatedSystem cpuType insnSet addrType wordType))
sysReadAndIncPC pc sys = (1+ pc, sysMRead (unPC pc) sys)

-- | Fetch the next word from memory (pre-incrementing the program counter), returning the (incremented pc, contents)
sysIncPCAndRead
  :: (Integral addrType, Integral wordType, DVU.Unbox wordType)
  => ProgramCounter addrType
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> (ProgramCounter addrType, (wordType, EmulatedSystem cpuType insnSet addrType wordType))
sysIncPCAndRead pc sys = (pc', sysMRead (unPC pc') sys) where pc' = 1+ pc

-- | Read a single word from a system's memory, updating the system's memory state as part of the return
sysMRead
  :: (Integral addrType, Integral wordType, DVU.Unbox wordType)
  => addrType
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> (wordType, EmulatedSystem cpuType insnSet addrType wordType)
sysMRead addr sys = views memory (M.mRead addr) sys & _2 %~ updateMemSys
  where
    updateMemSys msys = sys & memory .~ msys

-- | Read _n_ words starting at the program counter's current value, returning (final program counter, ([word vector], new
-- system state))
sysPCReadN
  :: ( Integral addrType
     , Integral wordType
     , DVU.Unbox wordType
     , Show addrType
     , PrintfArg addrType
     , PrintfArg wordType
     , Show wordType
     )
  => ProgramCounter addrType
  -> Int
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> (ProgramCounter addrType, (Vector wordType, EmulatedSystem cpuType insnSet addrType wordType))
sysPCReadN pc nwords sys = (pc + fromIntegral nwords, sysMReadN (unPC pc) nwords sys)

-- | Read _n_ words from memory starting at address 'addr'. Returns a vector of words read and the updated system
-- state.
sysMReadN
  :: ( Integral addrType
     , Integral wordType
     , DVU.Unbox wordType
     , PrintfArg addrType
     , PrintfArg wordType
     , Show addrType
     , Show wordType
     )
  => addrType
  -> Int
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> (Vector wordType, EmulatedSystem cpuType insnSet addrType wordType)
sysMReadN addr nwords sys = views memory (M.mReadN addr nwords) sys & _2 %~ updateMemSys
  where
    updateMemSys msys = sys & memory .~ msys


-- | Get the system CPU's instruction decoder (this actually occurs often enough that a
-- utility function is necessary.)

sysGetIDecoder :: EmulatedSystem cpuType insnSet addrType wordType
               -> ProcessorDecoder cpuType insnSet addrType wordType
sysGetIDecoder {-sys-} = view (processor . processorOps . idecode) {-sys-}

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
nullSystem :: (Ord addrType, Bounded addrType) => NullSystem addrType wordType
nullSystem = mkEmulatedSystem nullProcessor name aliases
 where
  name    = "Null/dummy system"
  aliases = ["null", "dummy"]

-- | The null processor
nullProcessor :: EmulatedProcessor NullCPU NullInsnSet addrType wordType
nullProcessor =
  EmulatedProcessor { _procPrettyName = "Null (dummy) processor", _cpu = NullCPU, _ops = ProcessorOps { _idecode = nullIDecode } }

-- | Null processor operations
{- instance ProcessorOps NullCPU NullInsnSet addrType wordType where
  idecode pc sys = (DecodedInsn pc NullNOP, sys) -}

nullIDecode :: ProcessorDecoder NullCPU NullInsnSet addrType wordType
nullIDecode pc sys = (DecodedInsn pc NullNOP, sys)
