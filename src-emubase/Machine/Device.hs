{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Device emulation.


-}
module Machine.Device
    ( -- * Fundamental device types and constructors
    {-  Device(..)
    , mkDevice -}
    -- * Device classes and types
      Device(..)
    , DeviceIO(..)
    , DevReaderFunc
    , DevWriterFunc
    -- * High level device functions
    , deviceRead
    , deviceWrite
    -- * Constant device
    , constDevice
    ) where

{-import           Control.Arrow              (second)-}
import Data.Functor.Identity
{-import           Control.Monad.State.Strict (State, StateT, execState, execStateT, runState, runStateT, state, liftM)-}
import           Control.Monad.State.Strict (StateT, execStateT, runStateT, state)

{-
-- | The emulated device type. This is simply a box around an existential type. Each `Device` must implement the `DeviceIO`
-- type class.
data DeviceM devM addrType wordType where
  DeviceM :: ( DeviceIO dev devM addrType wordType ) =>
             dev
          -- The (unboxed) device
          -> DeviceM devM addrType wordType

-- | Simple devices that don't require a special inner `Monad`; the monad is specialized to `Identity`
type Device addrType wordType = DeviceM Identity addrType wordType

-- Show instance:
instance Show (DeviceM devM addrType wordType) where
  show (DeviceM dev) = "Device " ++ show dev
-}

-- | Device-specific operations that don't depend on an address or a word type.
class (Monoid dev) => Device dev where
  -- | Device reset. The default implementation assumes the device type is a 'Monoid', ignoring the argument and
  -- returning 'mempty'.
  deviceReset      :: dev
                   -- ^ Original device state
                   -> dev
                   -- ^ Reset device state
  deviceReset _dev = mempty

-- | Type signature for general-purpose device reader functions, in which the inner `Monad` is something
-- other than `Identity` (e.g., a GUI)
type DevReaderFuncT dev devM addrType wordType = addrType -> StateT dev devM wordType

-- | Type signature for simplified device reader functions; the inner `Monad` (like `State` vs. `StateT`)
-- is `Identitiy`.
type DevReaderFunc dev addrType wordType = DevReaderFuncT dev Identity addrType wordType

-- | Type signature for gemeral-purpose device writer functions, in which the inner `Monad` is something
-- other than `Identity` (e.g., a GUI)
type DevWriterFuncT dev devM addrType wordType = addrType -> wordType -> StateT dev devM wordType

-- | Type signature for simplified device reader functions; the inner `Monad` (like `State` vs. `StateT`)
-- is `Identitiy`.
type DevWriterFunc dev addrType wordType = DevWriterFuncT dev Identity addrType wordType

-- | Device-specific operations that depend on an address and a word type. This type class is necessary because the
-- actual device `dev` in the `Device` type is existential; the type class unboxes `dev`.
class ( Show dev
      , Device dev
      , Monad devM
      ) =>
      DeviceIO dev devM addrType wordType where
  -- | Device reader function: Read a word (`wordType`) from the device at an address (`addrType`), potentially
  -- changing the device's state. Returns a '(word, newDeviceState)' pair, which is the same result as a `State`
  -- function.
  --
  -- Addresses are calculated relative to the base address of the memory region in which the device was created.
  -- Consequently, the address is an offset (0, 1, 2, 3, ...) rather than an aboslute address (0x1000, 0x1001, ...)
  --
  -- __Note__: Do not call this function directly, primarily because it is a `State` computation. Use `deviceRead`
  -- instead.
  readDeviceWord  :: DevReaderFuncT dev devM addrType wordType
  -- | Device writer function: Write a word (`wordType`) to the device at an address (`addrType`), which definitely
  -- changes the device's state. Returns the new device state.
  --
  -- Addresses are calculated relative to the base address of the memory region in which the device was created.
  -- Consequently, the address is an offset (0, 1, 2, 3, ...) rather than an aboslute address (0x1000, 0x1001, ...)
  --
  -- __Note__: Do not call this function directly, primarily because it is a `State` computation. Use `deviceWrite`
  -- instead.
  writeDeviceWord :: DevWriterFuncT dev devM addrType wordType

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Higher level device functions:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Read a word from a device, returning a `(value, updatedDevState)` pair result. This function invokes
-- `runState` to run the state forward. Addresses are calculated relative to the memory region's start, e.g.,
-- if the device's memory region starts at `0x1000` and the read address is `0x10ff`, then the address
-- passed to `deviceRead` is `0x00ff` (`0x10ff - 0x1000`).
deviceRead :: ( {-Monad devM, Device dev,-} DeviceIO dev devM addrType wordType ) =>
              addrType
           -- ^ Address to read from, relative to the memory region's start
           -> dev -- DeviceM devM addrType wordType
           -- ^ The memory-mapped device
           -> devM (wordType, dev)
           -- ^ Value/word read and updated device state pair
deviceRead addr = runStateT (readDeviceWord addr)

-- | Write a word to a device, returning the new device state as its result. This function invokes `execState`
-- to run the device's state forward, since only the state is important.
deviceWrite :: ( {-Monad devM, Device dev,-} DeviceIO dev devM addrType wordType ) =>
               addrType
            -- ^ Address being written to
            -> wordType
            -- ^ Value being written
            -> dev -- DeviceM devM addrType wordType
            -- ^ Current device state
            -> devM dev -- DeviceM devM addrType wordType
            -- ^ Result device state
deviceWrite addr word = execStateT (writeDeviceWord addr word)

{-
-- | Make a new device (wrapper around the `DeviceM` constructor)
mkDeviceM :: ( DeviceIO dev devM addrType wordType ) =>
             dev
          -> DeviceM devM addrType wordType
mkDeviceM = DeviceM

-- | Make a (simplified) device
mkDevice :: ( DeviceIO dev Identity addrType wordType ) =>
            dev
         -> Device addrType wordType
mkDevice = Device-}

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
instance (Num wordType) => Device (ConstDevice wordType)

-- Instantiate the DeviceIO class
instance ( Integral wordType
         , Show wordType
         ) =>
         DeviceIO (ConstDevice wordType) Identity addrType wordType where
  readDeviceWord _addr       = state constDeviceReader
  writeDeviceWord _addr word  = state (const (word, ConstDevice word))

constDeviceReader :: (Integral wordType) => ConstDevice wordType -> (wordType, ConstDevice wordType)
constDeviceReader dev@(ConstDevice x) = (fromIntegral x, dev)

constDevice :: (Num wordType) => ConstDevice wordType
constDevice = mempty
