{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- | Test devices for memory system testing.
-}
module Machine.Tests.TestDevice ( mkTestDevice ) where

import           Control.Monad.State.Strict (state)
import Data.Word
import           Data.Vector.Unboxed             (Vector, (!), (//))
import qualified Data.Vector.Unboxed             as DVU

import           Machine.Device

-- | A very simple counter device
newtype TestDevice = TestDevice Int
  deriving (Show)

instance Monoid TestDevice where
  -- Use non-zero starting value (since a memory system read will return 0 if an address cannot be read or isn't in a
  -- mapped region.)
  mempty = TestDevice 19
  (TestDevice a) `mappend` (TestDevice b) = TestDevice (a + b)

-- Instantiate the DeviceThings type class:
instance DeviceThings TestDevice

-- Instantiate the DeviceIO class
instance (Integral wordType) => DeviceIO TestDevice addrType wordType where
  deviceReader _addr       = state testDeviceReader
  deviceWriter _addr _word = state (\s -> (-1, s))

-- | Example device reader function that just increments the counter as the updated state. Slightly less overhead
-- than using MonadState 'get' and 'put'. The address is ignored.
testDeviceReader :: (Integral wordType) => TestDevice -> (wordType, TestDevice)
testDeviceReader (TestDevice x) = (fromIntegral x, TestDevice (x+ 1))

{- An alternative implementation that uses MonadState 'get' and 'put' (which is the actual implementation of `state`, BTW):

instance DeviceThings TestDevice where
  deviceReader = testDeviceReader
  ...

testDeviceReader :: (Integral wordType) => DevReaderFunc TestDevice addrType wordType
testDeviceReader _addr = get >>= (\x -> put (x + 1) >> return (fromIntegral x))
-}

-- | And finally, a factory constructor function.
mkTestDevice :: (Integral wordType) =>
                Device addrType wordType
mkTestDevice = Device (mempty :: TestDevice)

{- Test video device: -}
data VideoDevice where
  VideoDevice ::
    { _vidRAM :: Vector Word8
    } -> VideoDevice

instance Monoid VideoDevice where
  mempty = VideoDevice {
             _vidRAM = DVU.replicate (64 * 16) (0 :: Word8)
           }
  _ `mappend` vidB = vidB

instance DeviceThings VideoDevice

instance DeviceIO VideoDevice addrType wordType where
  deviceReader = undefined
  deviceWriter = undefined
