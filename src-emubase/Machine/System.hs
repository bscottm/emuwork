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

import           Control.Monad.Trans.State.Strict (StateT, state)
import           Data.Functor.Identity (Identity)
import           Lens.Micro.Platform
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
                  -- ^ Names by which the system is known (short forms that are easier to remember.)
    }
    deriving (Show)

-- | Lens on the 'EmulatedSystem'-s processor. This lens is read/write because the processor is stateful and gets updated.
-- This doesn't mean you should try to replace a Z80 with a Motorola 6502.
processor :: Lens' (EmulatedSystem cpuType insnSet addrType wordType) (EmulatedProcessor cpuType insnSet addrType wordType)
processor f sys = (\proc -> sys { _processor = proc }) <$> f (_processor sys)

-- | Lens on the 'EmulatedSystem'-s memory. This lens is read/write because the memory system is stateful and gets updated.
memory :: Lens' (EmulatedSystem cpuType insnSet addrType wordType) (M.MemorySystem addrType wordType)
memory f sys = (\msys -> sys { _memory = msys }) <$> f (_memory sys)

-- | Lens on the 'EmulatedSystem'-s full name. This lens is read-only.
sysName :: Lens' (EmulatedSystem cpuType addrType wordType insnSet) String
sysName f sys = (\name -> sys { _sysName = name }) <$> f (_sysName sys)

-- | Lens on the 'EmulatedSystem'-s list of aliases (short names). This lens is read-only.
sysAliases :: Lens' (EmulatedSystem cpuType addrType wordType insnSet) [String]
sysAliases f sys = (\aliases -> sys { _sysAliases = aliases }) <$> f (_sysAliases sys)

-- | Make an emulated system.
-- 
-- /NOTE:/ The initial memory system is not one of the parameters nor is it required to construct an emulated system.
-- 'mkEmulatedSystem' creates an initial, empty memory system for the system. It's up to you to customize the memory
-- system after you invoke 'mkEmulatedSystem'.
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
data EmulatedProcessor cpuType insnSet addrType wordType =
  EmulatedProcessor
    { _procPrettyName :: String
    -- ^ Pretty name for the emulated processor
    , _cpu            :: cpuType
    -- ^ Processor-specific internal data (registers, flags, etc.)
    , _ops            :: ProcessorOps cpuType insnSet addrType wordType
    -- ^ Processor operation functions
    }
    deriving (Show)

-- | Lens for the processor's pretty name. This is a read-only lens.
procPrettyName :: Lens' (EmulatedProcessor cpuType insnSet addrType wordType) String
procPrettyName f proc = proc <$ f (_procPrettyName proc)

-- | Lens for the processor's internals (the CPU). This is a read/write lens because the CPU is stateful and gets updated.
cpu :: Lens' (EmulatedProcessor cpuType insnSet addrType wordType) cpuType
cpu f proc = (\cpu' -> proc { _cpu = cpu' }) <$> f (_cpu proc)

-- | Lens for the processor operations (functions). This is a read-only lens.
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
  -- ^ The instruction to decode
  }

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
    _idecode :: ProcessorDecoder cpuType insnSet addrType wordType
  -- ^ Instruction decoder, for disassembly and execution
  , _iexecute :: InstructionExecute cpuType insnSet addrType wordType
  -- ^ Instruction execution function.
  }

-- | Lens for the instruction decoder function. Note that this is effectively "read-only" -- one cannot change the
-- instruction decoder function (and why would one do that?.)
idecode :: Lens' (ProcessorOps cpuType insnSet addrType wordType) (ProcessorDecoder cpuType insnSet addrType wordType)
idecode f decoder = decoder <$ f (_idecode decoder)

instance Show (ProcessorOps cpuType insnSet addrType wordType) where
  -- Nothing to show. Need the instance for the EmulatedSystem automagically derived Show instance.
  show ProcessorOps{} = ""

-- | Instruction decoder function type. The signature is set up for 'runState' and friends.
type ProcessorDecoder cpuType insnSet addrType wordType
  =  ProgramCounter addrType
  -- ^ Current program counter, from where instructions are fetched
  -> EmulatedSystem cpuType insnSet addrType wordType
  -- ^ The emulated system (uses the system's memory to read instruction and operands.)
  -> (DecodedInsn insnSet addrType, EmulatedSystem cpuType insnSet addrType wordType)
  -- ^ The decoded instruction and updated emulated system (because system's memory can change/get updated.)

-- | Instruction execution function type. The signature is set up for 'runState' and friends.
type InstructionExecute cpuType insnSet addrType wordType
  = DecodedInsn insnSet addrType
  -- ^ The decoded instruction to execute
  ->  EmulatedSystem cpuType insnSet addrType wordType
  -- ^ The emulated system (potentially uses the system's memory to make memory references within the instruction.)
  -> EmulatedSystem cpuType insnSet addrType wordType
  -- ^ The updated emulated system

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

-- | Read a word from memory, then increment the program counter. Returns the updated program counter, memory value
-- and updated system state as the tuple @(newPC, (val, updatedSystem))@.
sysReadAndIncPC
  :: (Integral addrType, Integral wordType, DVU.Unbox wordType)
  => ProgramCounter addrType
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> (ProgramCounter addrType, (wordType, EmulatedSystem cpuType insnSet addrType wordType))
sysReadAndIncPC pc sys = (1+ pc, sysMRead (unPC pc) sys)

-- | Fetch the next word from memory (pre-incrementing the program counter), returning the incremented pc, memory value
-- and updated system state as the tuple @(newPC, (val, updatedSystem))@.
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
sysMRead addr sys = sys ^. memory & M.mRead addr & _2 %~ updateMemSys
  where
    updateMemSys msys = sys & memory .~ msys

-- | 'sysMRead' adapted as a 'Control.Monad.Trans.State' state transformer.
stateSysMRead
  :: (Integral addrType, Integral wordType, DVU.Unbox wordType)
  => addrType
  -> StateT (EmulatedSystem cpuType0 insnSet0 addrType wordType) Identity wordType
stateSysMRead addr = state (sysMRead addr)

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
  :: ( Integral addrType, Integral wordType, DVU.Unbox wordType, PrintfArg addrType, PrintfArg wordType
     , Show addrType, Show wordType
     )
  => addrType
  -> Int
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> (Vector wordType, EmulatedSystem cpuType insnSet addrType wordType)
sysMReadN addr nwords sys = sys ^. memory & M.mReadN addr nwords & _2 %~ updateMemSys
  where
    updateMemSys msys = sys & memory .~ msys

-- | Write a 'word' value to memory at address 'addr'. Returns the updated system state.
sysMWrite
  :: (Integral addrType, Integral wordType, Show wordType, DVU.Unbox wordType, PrintfArg addrType
     , Show addrType , PrintfArg wordType , Show wordType)
  => addrType
  -> wordType
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> EmulatedSystem cpuType insnSet addrType wordType
sysMWrite addr word sys = sys & memory %~ M.mWrite addr word

-- | Adapt 'sysMWrite` to a 'Control.Monad.Trans.State' state transformer for 'execState' and friends.
stateSysMWrite
  :: (Integral addrType, Integral wordType, Show wordType, DVU.Unbox wordType, PrintfArg addrType
     , Show addrType , PrintfArg wordType , Show wordType)
  => addrType
  -> wordType
  -> StateT (EmulatedSystem cpuType1 insnSet1 addrType wordType) Identity ()
stateSysMWrite addr word = state (\sys -> ((), sysMWrite addr word sys))

sysMWriteN
  :: (Integral addrType, Integral wordType, Show wordType, DVU.Unbox wordType, PrintfArg addrType
     , Show addrType , PrintfArg wordType , Show wordType)
  => addrType
  -> Vector wordType
  -> EmulatedSystem cpuType insnSet addrType wordType
  -> EmulatedSystem cpuType insnSet addrType wordType
sysMWriteN addr vec sys = sys & memory %~ M.mWriteN addr vec

-- | Adapt 'sysMWriteN` to a 'Control.Monad.Trans.State' state transformer for 'execState' and friends.
stateSysMWriteN
  :: (Integral addrType, Integral wordType, Show wordType, DVU.Unbox wordType, PrintfArg addrType
     , Show addrType , PrintfArg wordType , Show wordType)
  => addrType
  -> Vector wordType
  -> StateT (EmulatedSystem cpuType1 insnSet1 addrType wordType) Identity ()
stateSysMWriteN addr word = state (\sys -> ((), sysMWriteN addr word sys))


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
  EmulatedProcessor
    { _procPrettyName = "Null (dummy) processor"
    , _cpu = NullCPU
    , _ops = ProcessorOps { _idecode = nullIDecode, _iexecute = nullExecute }
    }

nullIDecode :: ProcessorDecoder NullCPU NullInsnSet addrType wordType
nullIDecode pc sys = (DecodedInsn pc NullNOP, sys)

nullExecute :: InstructionExecute NullCPU NullInsnSet addrType wordType
nullExecute _insn sys = sys
