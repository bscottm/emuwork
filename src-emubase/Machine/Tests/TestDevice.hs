{-# LANGUAGE GADTs #-}

module Machine.Tests.TestDevice ( mkTestDevice ) where

import           Control.Arrow              ((***))
import           Control.Monad.State.Strict (get, put)

import           Machine.Device

newtype TestDevice = TestDevice Int
  deriving (Show)

{- Yech. The things we need to do in Haskell. -}
instance Eq TestDevice where
  (TestDevice x) == (TestDevice y) = x == y

instance Ord TestDevice where
  (TestDevice x) `compare` (TestDevice y) = x `compare` y

instance Num TestDevice where
  TestDevice x + TestDevice y = TestDevice (x + y)
  TestDevice x - TestDevice y = TestDevice (x - y)
  negate (TestDevice x)       = TestDevice (negate x)
  TestDevice x * TestDevice y = TestDevice (x * y)
  abs n                       = if n > 0 then n else negate n

  signum n | n < 0       = negate 1
           | n == 0      = 0
           | otherwise   = 1

  fromInteger i = TestDevice (fromInteger i)

instance Real TestDevice where
  toRational (TestDevice x) = toRational x

instance Integral TestDevice where
  toInteger (TestDevice x) = toInteger x
  quotRem (TestDevice x) (TestDevice y) = TestDevice *** TestDevice $ quotRem x y

instance Enum TestDevice where
  toEnum x                = TestDevice (toEnum x)
  fromEnum (TestDevice x) = fromEnum x

instance Monoid TestDevice where
  mempty = TestDevice 0
  (TestDevice a) `mappend` (TestDevice b) = TestDevice (a + b)

instance DeviceOps TestDevice

testDeviceReader :: (Integral wordType) => MemDevReader TestDevice addrType wordType
testDeviceReader _addr =
  do x <- get
     put (x + 1)
     return (fromIntegral (x + 1))

mkTestDevice :: (Integral wordType) => MemMappedDevice addrType wordType
mkTestDevice = MemMappedDevice testDeviceReader (mempty :: TestDevice)
