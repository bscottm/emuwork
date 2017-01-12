{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- | Test devices for memory system testing.
-}
module Machine.Tests.TestDevice ( mkTestDevice ) where

import           Control.Monad.State.Strict (get, put)

import           Machine.Device

-- | A very simple counter device, with a non-zero starting value (since a memory system read will return 0 if an address
-- cannot be read or isn't in a mapped region.)
newtype TestDevice = TestDevice Int
  deriving (Show, Num, Integral, Real, Enum, Eq, Ord)

instance Monoid TestDevice where
  mempty = TestDevice 19
  (TestDevice a) `mappend` (TestDevice b) = TestDevice (a + b)

instance DeviceThings TestDevice

instance (Integral wordType) =>
         DeviceIO TestDevice addrType wordType where
  deviceReader = testDeviceReader

testDeviceReader :: (Integral wordType) => DevReaderFunc TestDevice addrType wordType
testDeviceReader _addr = get >>= (\x -> put (x + 1) >> return (fromIntegral x))

mkTestDevice :: (Integral wordType) =>
                Device addrType wordType ioAddrType ioWordType
mkTestDevice = MemMappedDevice (mempty :: TestDevice)
