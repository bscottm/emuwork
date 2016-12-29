{-# LANGUAGE FlexibleInstances     #-}

{- | Device emulation.


-}
module Machine.Device
    ( mkDevice
    -- * High level device functions
    , deviceRead
    , deviceWrite
    , bulkDeviceWrite
    -- * Constant device
    , constDevice
    ) where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (execState, runState, state)
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as DVU

import           Machine.Types

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
deviceRead addr (Device dev) = second Device (runState (readDeviceWord addr) dev)

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
deviceWrite addr word (Device dev) = Device (execState (writeDeviceWord addr word) dev)

-- | Bulk device write: write a vecot of values to the device starting at offset `soffs` and incrementing the address
-- for each successive value.
--
-- NOTE: This function cannot (and does not) do any bounds checking! The underlying device implementing `deviceWrite`
-- needs to ensure that the offsets are valid.
bulkDeviceWrite :: (Num addrType
                   , DVU.Unbox wordType
                   ) =>
                   addrType
                -- ^ Starting offset
                -> Vector wordType
                -- ^ Values to write
                -> Device addrType wordType
                -- ^ Current device state
                -> Device addrType wordType
                -- ^ Result device state
bulkDeviceWrite soffs vals (Device dev) =
  let valPairs              = zip (take (DVU.length vals) (iterate (+ 1) soffs)) (DVU.toList vals)
      writeFunc (addr, val) = runState (writeDeviceWord addr val)
  in  Device (execState (traverse (state . writeFunc) valPairs) dev)

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

-- Instantiate the DeviceFuncs type class:
instance (Num wordType) => DeviceFuncs (ConstDevice wordType)

-- Instantiate the DeviceIO class
instance ( Integral wordType
         , Show wordType) =>
         DeviceIO (ConstDevice wordType) addrType wordType where
  readDeviceWord _addr       = state constDeviceReader
  writeDeviceWord _addr word  = state (const ((), ConstDevice word))

constDeviceReader :: (Integral wordType) => ConstDevice wordType -> (wordType, ConstDevice wordType)
constDeviceReader dev@(ConstDevice x) = (fromIntegral x, dev)

constDevice :: (Num wordType) => ConstDevice wordType
constDevice = mempty
