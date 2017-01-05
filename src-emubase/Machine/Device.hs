{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Machine.Device
    ( DeviceManager(..)
    , DeviceState(..)
    , DeviceOps(..)
    , MemMappedDevice(..)
    ) where

import Data.HashMap.Strict (HashMap)

data DeviceManager addrType portType where
  DeviceManager ::
    { _memDevices :: HashMap addrType (DeviceState dev)
    , _ioDevices  :: HashMap portType (DeviceState dev)
    } -> DeviceManager addrType portType

newtype DeviceState dev = DeviceState { unDev :: dev }
  deriving (Show)

instance Functor DeviceState where
  fmap f (DeviceState d) = DeviceState (f d)

instance Applicative DeviceState where
  pure = DeviceState
  DeviceState f <*> DeviceState d = DeviceState (f d)

instance Monad DeviceState where
  dev >>= k = k (unDev dev)

-- | Type class for basic operations operations on emulated devices.
class DeviceOps dev where
  -- | Reset the device to its power-on state
  reset :: DeviceState dev -> DeviceState dev

-- | Type class for operations on memory-mapped devices
class (DeviceOps dev) => MemMappedDevice dev addrType wordType where
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
