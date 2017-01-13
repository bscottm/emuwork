{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Emulated devices.


-}
module Machine.Device
    ( -- * Fundamental device types
      Device(..)
    -- * Device functions
    , DeviceThings(..)
    , DeviceIO(..)
    , DevReaderFunc
    , DevWriterFunc
    , deviceRead
    , deviceWrite
    ) where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (State, runState, execState)

-- | The emulated device type.
data Device addrType wordType where
  Device :: (DeviceIO dev addrType wordType, Show dev) =>
            dev
         -- The underlying device
         -> Device addrType wordType

-- Show instance:
instance Show (Device addrType wordType) where
  show (Device dev) = "Device " ++ show dev

-- | Device-specific operations that don't depend on an address or a word type.
class (Monoid dev) => DeviceThings dev where
  -- | Device reset. The default implementation assumes the device type is a 'Monoid', ignoring the argument and
  -- returning 'mempty'.
  deviceReset :: dev
              -- ^ Original device state
              -> dev
              -- ^ Reset device state
  deviceReset _dev = mempty

-- | Type signature for device reader functions
type DevReaderFunc dev addrType wordType = addrType -> State dev wordType

-- | Type signature for device writer functions
type DevWriterFunc dev addrType wordType = addrType -> wordType -> State dev wordType

-- | Device-specific operations that depend on an address and a word type. This type class is necessary because the
-- actual device `dev` in the `Device` type is existential; the type class unboxes `dev`.
class (DeviceThings dev) => DeviceIO dev addrType wordType where
  -- | Device reader function: Read a word (`wordType`) from the device at an address (`addrType`), potentially
  -- changing the device's state. Returns a '(word, newDeviceState)' pair, which is the same result as a `State`
  -- function.
  --
  -- __Note__: Do not call this function directly, primarily because it is a `State` computation. Use `deviceRead`
  -- instead.
  deviceReader :: DevReaderFunc dev addrType wordType
  -- | Device writer function: Write a word (`wordType`) to the device at an address (`addrType`), which definitely
  -- changes the device's state. Returns the new device state.
  --
  -- __Note__: Do not call this function directly, primarily because it is a `State` computation. Use `deviceWrite`
  -- instead.
  deviceWriter :: DevWriterFunc dev addrType wordType

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Memory-mapped devices:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Read a word from a device, returning a `(value, updatedDevState)` pair result. This function invokes
-- `runState` to run the state forward.
deviceRead :: addrType
           -- ^ Address to read from
           -> Device addrType wordType
           -- ^ The memory-mapped device
           -> (wordType, Device addrType wordType)
           -- ^ Value/word read and updated device state pair
deviceRead addr (Device dev) = second Device (runState (deviceReader addr) dev)

-- | Write a word to a device, returning the new device state as its result. This function invokes `execState`
-- to run the state forward, since only the state is important.
deviceWrite :: addrType
            -> wordType
            -> Device addrType wordType
            -> Device addrType wordType
deviceWrite addr word (Device dev) = Device (execState (deviceWriter addr word) dev)
