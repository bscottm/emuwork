{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- | Test devices for memory system testing.
-}
module Machine.Tests.TestDevice ( mkTestDevice ) where

import           Control.Monad.State.Strict (state)

import           Machine.Device

-- | A very simple counter device
newtype TestDevice = TestDevice Int
  deriving (Show, Num, Integral, Real, Enum, Eq, Ord)

instance Monoid TestDevice where
  -- Use non-zero starting value (since a memory system read will return 0 if an address cannot be read or isn't in a
  -- mapped region.)
  mempty = TestDevice 19
  (TestDevice a) `mappend` (TestDevice b) = TestDevice (a + b)

-- Instantiate the DeviceThings type class:
instance DeviceThings TestDevice

-- Instantiate the DeviceIO class
instance (Integral wordType) => DeviceIO TestDevice addrType wordType where
  deviceReader = testDeviceReader

-- | Example device reader function that just increments the counter as the updated state. Slightly less overhead
-- than using MonadState 'get' and 'put'. The address is ignored.
testDeviceReader :: (Integral wordType) => DevReaderFunc TestDevice addrType wordType
testDeviceReader _addr = state (\(TestDevice x) -> (fromIntegral x, TestDevice (x+ 1)))

{- An alternative implementation that uses MonadState 'get' and 'put':

testDeviceReader :: (Integral wordType) => DevReaderFunc TestDevice addrType wordType
testDeviceReader _addr = get >>= (\x -> put (x + 1) >> return (fromIntegral x))
-}

-- | And finally, a factory constructor function.
mkTestDevice :: (Integral wordType) =>
                Device addrType wordType
mkTestDevice = Device (mempty :: TestDevice)
