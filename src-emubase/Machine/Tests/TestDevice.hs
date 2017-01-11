{-# LANGUAGE GADTs #-}

module Machine.Tests.TestDevice ( mkTestDevice ) where

import           Control.Monad.State.Strict (get, put)

import Machine.Device

newtype TestDevice = TestDevice Int
  deriving (Show)

instance Monoid TestDevice where
  mempty = TestDevice 0
  (TestDevice a) `mappend` (TestDevice b) = TestDevice (a + b)

instance DeviceOps TestDevice

testDeviceReader :: (Integral wordType) => MemDevReader TestDevice addrType wordType
testDeviceReader _addr =
  do x <- get
     let (TestDevice x') = x
     put (TestDevice (x' + 1))
     return (fromIntegral (x' + 1))

mkTestDevice :: (Integral wordType) => Device addrType wordType
mkTestDevice = MemMappedDevice testDeviceReader (TestDevice 0)