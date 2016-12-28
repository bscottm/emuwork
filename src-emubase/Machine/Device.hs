{-# LANGUAGE MultiParamTypeClasses #-}

module Machine.Device
    ( DeviceOps(..)
    , MemMappedDeviceOps(..)
    ) where

import Control.Monad.IO.Class

-- | Type class for operations on emulated devices, i.e., operations that change the device's internal state
class (MonadIO devM) => DeviceOps devM where
  -- | Reset the device to its power-on state
  reset :: devM x -> devM x

-- | Type class for operations on memory-mapped devices
class (DeviceOps devM) => MemMappedDeviceOps devM addrType wordType where
  -- | Read a word from the device
  devRead  :: addrType
           -- ^ Address being read
           -> devM a
           -- ^ The current device state
           -> (wordType, devM b)
           -- ^ Read word, new device state
  -- | Write a word to a device
  devWrite :: addrType
           -- ^ Device's memory address being written to
           -> wordType
           -- ^ Value to write
           -> devM a
           -- ^ Current device state
           -> devM b
           -- ^ Resulting device state