{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Emulated devices.


-}
module Machine.Device
    ( -- * Fundamental device types
      MemMappedDevice(..)
    , IODevice(..)
    -- * Device functions
    , DeviceOps(..)
    , MemDevReader
    , IODevReader
    , memDevRead
    , ioRead
    ) where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (State, runState)

-- | Type class required to operate on boxed devices
class (Monoid dev) => DeviceOps dev where
  -- | Device reset. The default assumes the device type is a 'Monoid', ignoring the argument and returning 'mempty'.
  deviceReset :: dev
              -- ^ Original device state
              -> dev
              -- ^ Reset device state
  deviceReset _dev = mempty

-- | The emulated device type, in two varieties: `MemMappedDevice` and `IODevice`. `MemMappedDevice` is for 
-- memory-mapped devices, whereas `IODevice` is for Zilog- and Intel-type processors, which have a separate
-- I/O address space.
data MemMappedDevice addrType wordType where
  MemMappedDevice :: (DeviceOps dev, Show dev) =>
                     MemDevReader dev addrType wordType
                  -- ^ The reader embedded state transform function
                  -> dev
                  -- ^ The underlying device
                  -> MemMappedDevice addrType wordType

data IODevice ioAddrType ioWordType where
  IODevice        :: (DeviceOps dev, Show dev) =>
                     IODevReader dev ioAddrType ioWordType
                  -> dev
                  -> IODevice ioAddrType ioWordType

-- | Type signature for reading a memory-mapped device
type MemDevReader dev addrType wordType = addrType -> State dev wordType

-- | Type signature for reading an I/O device
type IODevReader dev ioAddrType ioWordType = ioAddrType -> State dev ioWordType

-- Show instance:
instance Show (MemMappedDevice addrType wordType) where
  show (MemMappedDevice _ dev) = "MemMappedDevice " ++ show dev

-- Show instance:
instance Show (IODevice ioAddrType ioWordType) where
  show (IODevice _ dev)        = "IODevice " ++ show dev

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Memory-mapped devices:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Read a word from a memory-mapped device
memDevRead :: addrType
           -- ^ Address to read from
           -> MemMappedDevice addrType wordType
           -- ^ The memory-mapped device
           -> (wordType, MemMappedDevice addrType wordType)
           -- ^ Value/word read and updated device state pair
memDevRead addr (MemMappedDevice reader dev) =
  let updateDev = MemMappedDevice reader
  in  second updateDev (runState (reader addr) dev)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- I/O port devices: These are distinct from memory-mapped because they live in an entirely different
-- "memory" address space (e.g., Zilog and Intel port I/O)
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

ioRead :: ioAddrType
       -- ^ Address to read from
       -> IODevice ioAddrType ioWordType
       -- ^ The memory-mapped device
       -> (ioWordType, IODevice ioAddrType ioWordType)
       -- ^ Value/word read and updated device state pair
ioRead port (IODevice reader dev) =
  let updateDev = IODevice reader
  in  second updateDev (runState (reader port) dev)
