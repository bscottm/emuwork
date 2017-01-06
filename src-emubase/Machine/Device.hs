{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Emulated devices.


-}
module Machine.Device
    ( -- * Fundamental device types
      Device(..)
    , MemMappedDevice(..)
    -- * Device functions
    , DeviceOps(..)
    , memDevRead
    ) where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (State, runState)

-- | Boxed device type to cope with the existential 'dev' type.
data Device where
  Device :: (DeviceOps dev) =>
            dev
         -> Device

-- | Type class required to operate on boxed devices
class (Monoid dev) => DeviceOps dev where
  -- | Device reset. The default assumes the device type is a 'Monoid', ignoring the argument and returning 'mempty'.
  deviceReset :: dev
              -- ^ Original device state
              -> dev
              -- ^ Reset device state
  deviceReset _dev = mempty

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Memory-mapped devices:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Type signature for reading a memory-mapped device
type MemDevReader addrType wordType = addrType -> State Device wordType

-- | Memory mapped device's internal state. This is a container for the reader function ('MemDevReader'), the writer
-- function and the actual underlying device itself. It is also a place where the underlying device's state ('_device')
-- can be updated when reading or writing to the device.
data MemMappedDevice addrType wordType where
  -- | A memory-mapped device
  MemMappedDevice ::
    { _reader :: MemDevReader addrType wordType
    -- ^ The reader embedded state transform function
    , _device :: Device
    -- ^ The underlying device
    } -> MemMappedDevice addrType wordType

instance Show (MemMappedDevice addrType wordType) where
  show dev = "MemMappedDevice " ++ show dev

-- | Read a word from a memory-mapped device
memDevRead :: addrType
           -- ^ Address to read from
           -> MemMappedDevice addrType wordType
           -- ^ The memory-mapped device
           -> (wordType, MemMappedDevice addrType wordType)
           -- ^ Value/word read and updated device state pair
memDevRead addr dev = let updateDev dev' = dev { _device = dev' }
                      in  second updateDev (runState (_reader dev addr) (_device dev))

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- I/O port devices: These are distinct from memory-mapped because they live in an entirely different
-- "memory" address space (e.g., Zilog and Intel port I/O)
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
