{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Machine.Device
    ( DeviceState(..)
    , DeviceOps(..)
    , MemMappedDeviceOps(..)
    ) where

newtype DeviceState dev = DeviceState dev

-- | Type class for basic operations operations on emulated devices.
class DeviceOps dev where
  -- | Reset the device to its power-on state
  reset :: DeviceState dev -> DeviceState dev

-- | Type class for operations on memory-mapped devices
class (DeviceOps dev) => MemMappedDeviceOps dev addrType wordType where
  -- | Read a word from the device
  devRead  :: addrType
           -- ^ Address being read
           -> DeviceState dev
           -- ^ The current device state
           -> (wordType, DeviceState dev)
           -- ^ Read word, new device state
  -- | Write a word to a device
  devWrite :: addrType
           -- ^ Device's memory address being written to
           -> wordType
           -- ^ Value to write
           -> DeviceState dev
           -- ^ Current device state
           -> DeviceState dev
           -- ^ Resulting device state