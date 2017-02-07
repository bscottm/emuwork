{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | Machine emulation data types.
-}

module Machine.Types where

import           Control.Lens
import           Control.Monad.State.Strict    (State)
import           Data.Data
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as H
import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.IntervalMap.Interval     as I
import qualified Data.OrdPSQ                   as OrdPSQ
import qualified Data.Text                     as T
import           Data.Vector.Unboxed           (Vector)
import qualified Data.Vector.Unboxed           as DVU

import           Machine.ProgramCounter
import           Machine.Utils

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem cpuType instructionSet addrType wordType where
  EmulatedSystem ::
    { _processor  :: EmulatedProcessor cpuType addrType instructionSet
                  -- ^ System processor
    , _memory     :: MemorySystem addrType wordType
                     -- ^ System memory
    , _devices    :: DeviceManager addrType wordType
    -- ^ Container for memory-mapped devices, indexed by an integer identifier
    , _nDevices   :: Int
    -- ^ Number of devices (and device identifier generator)
    , _sysName    :: String
                  -- ^ The system's name, e.g. "Null/dummy processor"
    , _sysAliases :: [String]
                  -- ^ Names the system is known by.
    } -> EmulatedSystem cpuType instructionSet addrType wordType
    deriving (Show)

-- Need to manually generate the lenses due to the constraint on EmulatedSystem

processor :: Lens' (EmulatedSystem cpuType insnSet addrType wordType) (EmulatedProcessor cpuType addrType insnSet)
processor f sys = (\proc -> sys { _processor = proc }) <$> f (_processor sys)

memory :: Lens' (EmulatedSystem cpuType insnSet addrType wordType) (MemorySystem addrType wordType)
memory f sys = (\msys -> sys { _memory = msys }) <$> f (_memory sys)

-- | Lens for the memory-mapped device container
deviceTable :: Lens' (EmulatedSystem cpuType insnSet addrType wordType) (DeviceManager addrType wordType)
deviceTable f msys = (\memD -> msys { _devices = memD }) <$> f (_devices msys)

-- | Lens for the memory-mapped device container
nDevices :: Lens' (EmulatedSystem cpuType addrType wordType insnSet) Int
nDevices f msys = (\n -> msys { _nDevices = n }) <$> f (_nDevices msys)

sysName :: Lens' (EmulatedSystem cpuType addrType wordType insnSet) String
sysName f sys = (\name -> sys { _sysName = name }) <$> f (_sysName sys)

sysAliases :: Lens' (EmulatedSystem cpuType addrType wordType insnSet) [String]
sysAliases f sys = (\aliases -> sys { _sysAliases = aliases }) <$> f (_sysAliases sys)

-- | Emulated system constructor.
mkEmulatedSystem :: EmulatedProcessor cpuType addrType instructionSet
                 -- ^ Processor (CPU)
                 -> String
                 -- ^ System name
                 -> [String]
                 -- ^ System aliases
                 -> EmulatedSystem cpuType instructionSet addrType wordType
mkEmulatedSystem sysCpu name aliases = EmulatedSystem { _processor = sysCpu
                                                      , _memory = initialMemorySystem
                                                      , _devices  = H.empty
                                                      , _nDevices = 0
                                                      , _sysName = name
                                                      , _sysAliases = aliases
                                                      }

-- | 'EmulatedProcessor' boxes the emulated machine's processor. All interesting operations on an @EmulatedProcessor@
-- are in the `ProcessorOps` type class.
data EmulatedProcessor cpuType addrType instructionSet where
  EmulatedProcessor ::
    { _procPrettyName :: String
    -- ^ Pretty name for the emulated processor
    , _cpu            :: cpuType
    -- ^ Processor-specific internal data.
    } -> EmulatedProcessor  cpuType addrType instructionSet
    deriving (Show)

-- | Lens for the processor's pretty name
procPrettyName :: Lens' (EmulatedProcessor cpuType insnSet addrType) String
procPrettyName f proc = (\name' -> proc { _procPrettyName = name' }) <$> f (_procPrettyName proc)

-- | Lens for the processor's internals (the CPU).
cpu :: Lens' (EmulatedProcessor cpuType addrType insnSet) cpuType
cpu f proc = (\cpu' -> proc { _cpu = cpu' }) <$> f (_cpu proc)

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
class ProcessorOps cpuType insnSet addrType wordType where
  -- | Instruction decoder, for disassembly and execution
  idecode :: ProgramCounter addrType
          -- ^ Current program counter, from where instructions are fetched
          -> EmulatedSystem cpuType insnSet addrType wordType
          -- ^ The memory system
          -> (DecodedInsn insnSet addrType, EmulatedSystem cpuType insnSet addrType wordType)
          -- ^ The decoded instruction

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Memory system:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

{- | `MemorySystem` provides a unified interface to an emulated memory system: RAM, ROM and devices that occupy memory
address ranges. Note, though, that port-based I/O is also a type of memory system (see the `IOSystem` type alias), where
the addresses are port numbers.

Devices change state when read or written. Consequently, `MemorySystem` has a device table to maintain consistent device
state, which is updated after a read or a write. Reads and writes all return the updated memory system, with which the
caller has to use or reuse on subsequent memory system reads and writes.

`MemorySystem` is also part of the `Monoid` class, which makes it possible to merge memory systems together via
'mappend' or -- 'mconcat', so long as the memory systems' regions do not overlap with each other. Regions in the merged
memory system are distinct; they are not coalesced.
-}
data MemorySystem addrType wordType where
  MSys ::
    { _regions    :: MemRegionMap addrType wordType
    -- ^ Region interval map
    } -> MemorySystem addrType wordType

instance (Show addrType, Show wordType, DVU.Unbox wordType) =>
         Show (MemorySystem addrType wordType) where
  show msys = "MemorySystem " ++ show (msys ^. regions)

-- Admit MemorySystem into the Monoid class
instance (Ord addrType) => Monoid (MemorySystem addrType wordType) where
  mempty              = initialMemorySystem
  msysA `mappend` msysB
    | IM.null (IM.intersection (msysA ^. regions) (msysB ^. regions))
    = over regions (mappend (msysB ^. regions )) msysA
    | otherwise
    = error "MemorySystem:mappend: Overlapping memory regions between memory systems"

-- | Lens for the memory region map inside a 'MemorySystem'
regions :: Lens' (MemorySystem addrType wordType) (MemRegionMap addrType wordType)
regions f msys = (\regions' -> msys { _regions = regions' }) <$> f (_regions msys)

-- | Create an empty memory system (alternately: 'mempty')
initialMemorySystem :: MemorySystem addrType wordType
initialMemorySystem = MSys { _regions  = IM.empty }

-- | Shorthand for the memory region interval map
type MemRegionMap addrType wordType = IM.IntervalMap (I.Interval addrType) (MemoryRegion addrType wordType)
-- | Shorthand for the device manager
type DeviceManager addrType wordType = HashMap Int (Device addrType wordType)

-- | I/O system based on ports, e.g., Zilog and Intel, are an address space of their own, which makes
-- a type alias for `MemorySystem` appropriate. Port-based I/O systems use ports for their address space and this
-- results in a better code re-use.
type IOSystem portType wordType = MemorySystem portType wordType

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | A memory region
data MemoryRegion addrType wordType where
  RAMRegion ::
    { _contents       :: Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    , _writesPending  :: LRUWriteCache addrType wordType
    -- ^ Pending write LRU cache
    , _nWritesPending :: {-# UNPACK #-} !Int
    -- ^ Size of the LRU cache (avoids the $O(n)$ penalty for calling 'OrdPSQ.size')
    } -> MemoryRegion addrType wordType
  ROMRegion ::
    { _contents       :: Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    } -> MemoryRegion addrType wordType
  DevMemRegion :: Int
               -- Index into the memory system's '_memDevices' memory-mapped device container
               -> MemoryRegion addrType wordType
  deriving (Show)

-- | Lens for a memory region's contents
contents :: (DVU.Unbox wordType) =>
            Lens' (MemoryRegion addrType wordType) (Vector wordType)
contents f mregion@(RAMRegion ctnt _ _) = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion@(ROMRegion ctnt)     = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion@DevMemRegion{}       = mregion <$ f DVU.empty

-- | Lens for the pending write LRU queue.
writesPending :: Lens' (MemoryRegion addrType wordType) (LRUWriteCache addrType wordType)
writesPending f mregion@RAMRegion{}    = (\wp -> mregion { _writesPending = wp }) <$> f (_writesPending mregion)
-- For types other than 'RAMRegion', it's just the original memory region unchanged.
writesPending f mregion@ROMRegion{}    = mregion <$ f (initialLRU 0)
writesPending f mregion@DevMemRegion{} = mregion <$ f (initialLRU 0)

-- | Lens for number of elements in the LRU queue
nWritesPending :: Lens' (MemoryRegion addrType wordType) Int
nWritesPending f mregion@RAMRegion{} = (\n -> mregion { _nWritesPending = n }) <$> f (_nWritesPending mregion)
nWritesPending f mregion             = mregion <$ f 0

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- LRU pending write cache implementation, adapted from the psqueues example source. The pending write cache will
-- not exceed a given size.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

type Tick = Int
type Size = Int

data LRUWriteCache addrType wordType where
  LRUWriteCache ::
    { _lrucNextTick :: {-# UNPACK #-} !Tick
    , _lrucPsq      ::                !(OrdPSQ.OrdPSQ addrType Tick wordType)
    , _lrucMaxSize  :: {-# UNPACK #-} !Size
    } -> LRUWriteCache addrType wordType
    deriving (Show)

lrucPsq :: Lens' (LRUWriteCache addrType wordType) (OrdPSQ.OrdPSQ addrType Tick wordType)
lrucPsq f psq = (\psq' -> psq { _lrucPsq = psq' }) <$> f (_lrucPsq psq)

-- | Create an empty write cache
initialLRU :: Int
         -- ^ Maximum size of the pending write cache
         -> LRUWriteCache addrType wordType
         -- ^ The initial write cache
-- eta reduction: implied first argument, maxSize
initialLRU = LRUWriteCache 0 OrdPSQ.empty
{-# INLINE initialLRU #-}

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- Devices:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

-- | The emulated device type. This is a box around an existential type. Each `Device` must implement the `Device`,
-- `DeviceIO` type class.
data Device addrType wordType where
  Device :: ( DeviceIO dev addrType wordType ) =>
            dev
         -- The underlying device
         -> Device addrType wordType

-- Show instance:
instance Show (Device addrType wordType) where
  show (Device dev) = "Device " ++ show dev

-- | Device-specific operations that don't depend on an address or a word type.
class (Monoid dev) => DeviceFuncs dev where
  -- | Device reset. The default implementation assumes the device type is a 'Monoid', ignoring the argument and
  -- returning 'mempty'.
  deviceReset      :: dev
                   -- ^ Original device state
                   -> dev
                   -- ^ Reset device state
  deviceReset _dev = mempty

-- | Type signature for device reader functions
type DevReaderFunc dev addrType wordType = addrType -> State dev wordType

-- | Type signature for device writer functions
type DevWriterFunc dev addrType wordType = addrType -> wordType -> State dev ()

-- | Device-specific operations that depend on an address and a word type. This type class is necessary because the
-- actual device `dev` in the `Device` type is existential; the type class unboxes `dev`.
class ( Show dev
      , DeviceFuncs dev
      ) =>
      DeviceIO dev addrType wordType where
  -- | Device reader function: Read a word (`wordType`) from the device at an address (`addrType`), potentially
  -- changing the device's state. Returns a '(word, newDeviceState)' pair, which is the same result as a `State`
  -- function.
  --
  -- Addresses are calculated relative to the base address of the memory region in which the device was created.
  -- Consequently, the address is an offset (0, 1, 2, 3, ...) rather than an aboslute address (0x1000, 0x1001, ...)
  --
  -- __Note__: Do not call this function directly, primarily because it is a `State` computation. Use `deviceRead`
  -- instead.
  readDeviceWord  :: DevReaderFunc dev addrType wordType
  -- | Device writer function: Write a word (`wordType`) to the device at an address (`addrType`), which definitely
  -- changes the device's state. Returns the new device state.
  --
  -- Addresses are calculated relative to the base address of the memory region in which the device was created.
  -- Consequently, the address is an offset (0, 1, 2, 3, ...) rather than an aboslute address (0x1000, 0x1001, ...)
  --
  -- __Note__: Do not call this function directly, primarily because it is a `State` computation. Use `deviceWrite`
  -- instead.
  writeDeviceWord :: DevWriterFunc dev addrType wordType

-- | Simple device "system": For devices that have memory, such as video, there needs to be an encapsulating "system"
-- that manages the memory. This design may seem a little weird for relatively dumb devices (memory-mapped video), but
-- allows for more intelligent devices that have subdevices and processors (GPUs and sound cards.)
--
-- Note the phantom type that permits stronger type discrimination so that video devices can't inadvertantly be mixed
-- with disk drives.
type SimpleDeviceSystem devTag addrType wordType = NullSystem addrType wordType

-- | Constructor for `SimpleDeviceSystem`
mkSimpleDeviceSystem :: String
                     -> SimpleDeviceSystem devTag addrType wordType
mkSimpleDeviceSystem name = mkEmulatedSystem nullProcessor name []

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
nullProcessor :: EmulatedProcessor NullCPU addrType NullInsnSet
nullProcessor = EmulatedProcessor { _procPrettyName = "Null (dummy) processor"
                                  , _cpu            = NullCPU
                                  }

-- | Null processor operations
instance ProcessorOps NullCPU NullInsnSet addrType wordType where
  idecode pc sys = (DecodedInsn pc NullNOP, sys)
