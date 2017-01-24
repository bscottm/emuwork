{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Device emulation.


-}
module Machine.Device
    ( -- * Fundamental device types and constructors
      Device(..)
    , mkDevice
    -- * Device classes and types
    , DeviceThings(..)
    , DeviceIO(..)
    , DevReaderFunc
    , DevWriterFunc
    -- * High level device functions
    , deviceRead
    , deviceWrite
    -- * Constant device
    , constDevice
    ) where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (State, execState, runState, state)

-- | The emulated device type. This is simply a box around an existential type. Each `Device` must implement the `DeviceIO`
-- type class.
data Device addrType wordType where
  Device :: ( DeviceIO dev addrType wordType ) =>
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
class ( Show dev
      , DeviceThings dev
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
  deviceReader :: DevReaderFunc dev addrType wordType
  -- | Device writer function: Write a word (`wordType`) to the device at an address (`addrType`), which definitely
  -- changes the device's state. Returns the new device state.
  --
  -- Addresses are calculated relative to the base address of the memory region in which the device was created.
  -- Consequently, the address is an offset (0, 1, 2, 3, ...) rather than an aboslute address (0x1000, 0x1001, ...)
  --
  -- __Note__: Do not call this function directly, primarily because it is a `State` computation. Use `deviceWrite`
  -- instead.
  deviceWriter :: DevWriterFunc dev addrType wordType

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Higher level device functions:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Read a word from a device, returning a `(value, updatedDevState)` pair result. This function invokes
-- `runState` to run the state forward. Addresses are calculated relative to the memory region's start, e.g.,
-- if the device's memory region starts at `0x1000` and the read address is `0x10ff`, then the address
-- passed to `deviceRead` is `0x00ff` (`0x10ff - 0x1000`).
deviceRead :: addrType
           -- ^ Address to read from, relative to the memory region's start
           -> Device addrType wordType
           -- ^ The memory-mapped device
           -> (wordType, Device addrType wordType)
           -- ^ Value/word read and updated device state pair
deviceRead addr (Device dev) = second Device (runState (deviceReader addr) dev)

-- | Write a word to a device, returning the new device state as its result. This function invokes `execState`
-- to run the device's state forward, since only the state is important.
deviceWrite :: addrType
            -- ^ Address being written to
            -> wordType
            -- ^ Value being written
            -> Device addrType wordType
            -- ^ Current device state
            -> Device addrType wordType
            -- ^ Result device state
deviceWrite addr word (Device dev) = Device (execState (deviceWriter addr word) dev)

-- | Make a new device (wrapper around the `Device` constructor)
mkDevice :: ( DeviceIO dev addrType wordType ) =>
            dev
         -> Device addrType wordType
mkDevice = Device

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Constant device:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | A very simple constant device: Always returns the same value when read, stores a new value when
-- written
newtype ConstDevice wordType = ConstDevice wordType
  deriving (Show)

instance (Num wordType) => Monoid (ConstDevice wordType) where
  mempty                                    = ConstDevice 0
  (ConstDevice a) `mappend` (ConstDevice b) = ConstDevice (a + b)

-- Instantiate the DeviceThings type class:
instance (Num wordType) => DeviceThings (ConstDevice wordType)

-- Instantiate the DeviceIO class
instance ( Integral wordType
         , Show wordType) =>
         DeviceIO (ConstDevice wordType) addrType wordType where
  deviceReader _addr       = state constDeviceReader
  deviceWriter _addr word  = state (const (word, ConstDevice word))

constDeviceReader :: (Integral wordType) => ConstDevice wordType -> (wordType, ConstDevice wordType)
constDeviceReader dev@(ConstDevice x) = (fromIntegral x, dev)

constDevice :: (Num wordType) => ConstDevice wordType
constDevice = mempty
