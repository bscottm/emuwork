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
    , memMappedDevRead
    , ioDevRead
    ) where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (State, runState)

import Machine.Utils

-- | The emulated device type, in two varieties: `MemMappedDevice` and `IODevice`. `MemMappedDevice` is for 
-- memory-mapped devices, whereas `IODevice` is for Zilog- and Intel-type processors, which have a separate
-- I/O address space.
data Device addrType wordType ioPortType ioWordType where
  MemMappedDevice :: (DeviceIO dev addrType wordType, Show dev) =>
                     dev
                  -- The underlying device
                  -> Device addrType wordType ioPortType ioWordType

  IODevice        :: (DeviceIO dev ioPortType ioWordType, Show dev) =>
                     dev
                  -> Device ioAddrType ioWordType ioPortType ioWordType

-- Show instance:
instance Show (Device addrType wordType ioPortType ioWordType) where
  show (MemMappedDevice dev) = "MemMappedDevice " ++ show dev
  show (IODevice dev)        = "IODevice " ++ show dev

-- | Device-specific operations that don't depend on an address or a word type.
class (Monoid dev) => DeviceThings dev where
  -- | Device reset. The default assumes the device type is a 'Monoid', ignoring the argument and returning 'mempty'.
  deviceReset :: dev
              -- ^ Original device state
              -> dev
              -- ^ Reset device state
  deviceReset _dev = mempty

-- | Type signature for device reader functions
type DevReaderFunc dev addrType wordType = addrType -> State dev wordType

-- | Device-specific operations that depend on an address and a word type.
class (DeviceThings dev) => DeviceIO dev addrType wordType where
  -- | Device reader function: Read a word (`wordType`) from the device at an address (`addrType`), potentially
  -- changing the device's state. Returns the `(word, newDeviceState)` pair.
  deviceReader :: DevReaderFunc dev addrType wordType

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Memory-mapped devices:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Read a word from a device
memMappedDevRead :: (ShowHex addrType) =>
                    addrType
                 -- ^ Address to read from
                 -> Device addrType wordType ioAddrType ioPortType
                 -- ^ The memory-mapped device
                 -> (wordType, Device addrType wordType ioAddrType ioPortType)
                 -- ^ Value/word read and updated device state pair
memMappedDevRead addr (MemMappedDevice dev) = second MemMappedDevice (runState (deviceReader addr) dev)
memMappedDevRead addr _                     = error ("Non mem-mapped device read @" ++ as0xHexS addr)

ioDevRead :: (ShowHex ioAddrType) =>
             ioAddrType
             -- ^ Address to read from
             -> Device addrType wordType ioAddrType ioPortType
             -- ^ The memory-mapped device
             -> (wordType, Device addrType wordType ioAddrType ioPortType)
             -- ^ Value/word read and updated device state pair
ioDevRead port (IODevice dev)        = second IODevice (runState (deviceReader port) dev)
ioDevRead port _                     = error ("Non I/O device read @" ++ as0xHexS port)