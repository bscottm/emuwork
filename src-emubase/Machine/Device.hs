{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Emulated devices.


-}
module Machine.Device
    ( -- * Fundamental device types
      Device(..)
    , isMemMappedDevice
    , maybeMemMappedDevice
    -- * Device functions
    , DeviceOps(..)
    , MemDevReader
    , memDevRead
    ) where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (State, runState)

-- | Boxed device type to cope with the existential 'dev' type.
data Device addrType wordType where
  MemMappedDevice :: (DeviceOps dev, Show dev) =>
                     MemDevReader dev addrType wordType
                  -- ^ The reader embedded state transform function
                  -> dev
                  -- ^ The underlying device
                  -> Device addrType wordType

-- Show instance:
instance Show (Device addrType wordType) where
  show (MemMappedDevice _ dev) = "Device " ++ show dev

-- | Type class required to operate on boxed devices
class (Monoid dev) => DeviceOps dev where
  -- | Device reset. The default assumes the device type is a 'Monoid', ignoring the argument and returning 'mempty'.
  deviceReset :: dev
              -- ^ Original device state
              -> dev
              -- ^ Reset device state
  deviceReset _dev = mempty

-- | Predicate for memory mapped devices
isMemMappedDevice :: Device addrType wordType
                  -> Bool
isMemMappedDevice MemMappedDevice{} = True
isMemMappedDevice _                 = False

maybeMemMappedDevice :: Device addrType wordType
                     -> Maybe (Device addrType wordType)
maybeMemMappedDevice dev@MemMappedDevice{} = Just dev
maybeMemMappedDevice _                     = Nothing
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Memory-mapped devices:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Type signature for reading a memory-mapped device
type MemDevReader dev addrType wordType = addrType -> State dev wordType

-- | Read a word from a memory-mapped device
memDevRead :: addrType
           -- ^ Address to read from
           -> Device addrType wordType
           -- ^ The memory-mapped device
           -> (wordType, Device addrType wordType)
           -- ^ Value/word read and updated device state pair
memDevRead addr (MemMappedDevice reader dev) =
  let updateDev = MemMappedDevice reader
  in  second updateDev (runState (reader addr) dev)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- I/O port devices: These are distinct from memory-mapped because they live in an entirely different
-- "memory" address space (e.g., Zilog and Intel port I/O)
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
