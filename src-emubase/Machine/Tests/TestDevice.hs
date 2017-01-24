{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Test devices for memory system testing.
-}
module Machine.Tests.TestDevice
        ( mkTestDevice
        , mkVideoDevice
        ) where

import           Control.Arrow              (second)
import           Control.Lens               (Lens', set, (^.), (&), (%~))
import           Control.Monad.State.Strict (state)
import           Data.Word
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as DVU

import           Machine.Device
import qualified Machine.MemorySystem       as M
import           Machine.Utils

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
--
-- An alternative implementation that uses MonadState 'get' and 'put' (which is the actual implementation of `state`, BTW):
--
-- > instance (Integral wordType) => DeviceIO TestDevice addrType wordType where
-- >   deviceReader = testDeviceReader
-- >
-- > testDeviceReader :: (Integral wordType) => DevReaderFunc TestDevice addrType wordType
-- >testDeviceReader _addr = get >>= (\x -> put (x + 1) >> return (fromIntegral x))
--
testDeviceReader :: (Integral wordType) => TestDevice -> (wordType, TestDevice)
testDeviceReader (TestDevice x) = (fromIntegral x, TestDevice (x + 1))

-- | And finally, a factory constructor function.
mkTestDevice :: (Integral wordType) => Device addrType wordType
mkTestDevice = mkDevice (mempty :: TestDevice)

-- | Test video device. This exists primarily to test `MemorySystem` reusability. Also, the video device has
-- a specific address type (`Word16`) and word type (`Word8`). So, if the emulated machine's memory system
-- doesn't have the same `Word16`/`Word8` address and word types, one would have to create a new `DeviceIO`
-- instance that does the appropriate address and word type conversions (`fromIntegral`). (Note: This is
-- what an interface card would have to do anyway... except one has to do it explicitly here.)
data VideoDevice where
  VideoDevice ::
    { _vidRAM   :: VideoRAM
    } -> VideoDevice
  deriving (Show)

-- | Underlying video memory is a `Word16` address, `Word8` word type memory system.
type VideoRAM = M.MemorySystem Word16 Word8

-- | Getter/setter lens for _vidRAM
vidRAM :: Lens' VideoDevice VideoRAM
vidRAM f vdev = (\vram -> vdev { _vidRAM = vram}) <$> f (_vidRAM vdev)

-- | The `Monoid` instance
instance Monoid VideoDevice where
  mempty           = VideoDevice {
                      _vidRAM   = M.mkRAMRegion 0 vidLinearSize M.initialMemorySystem
                     }
  _ `mappend` vidB = vidB

-- | Default instance for `DeviceThings`
instance DeviceThings VideoDevice

-- | `DeviceIO` for the video memory
instance DeviceIO VideoDevice Word16 Word8 where
  deviceReader      = state . videoReader
  deviceWriter addr = state . videoWriter addr

-- | Number of rows for this video device
vidRows :: Int
vidRows = 24
--- | Number of columns for this video device
vidCols :: Int
vidCols = 64
-- | Linear size of the video RAM
vidLinearSize :: Int
vidLinearSize = vidRows * vidCols

-- | Read a byte from the video device's memory
videoReader :: Word16
            -> VideoDevice
            -> (Word8, VideoDevice)
videoReader addr vdev = second updVidRAM (M.mRead addr (vdev ^. vidRAM))
  where
    updVidRAM vram = set vidRAM vram vdev

-- | Write a byte to the video device's memory
videoWriter :: Word16
            -> Word8
            -> VideoDevice
            -> (Word8, VideoDevice)
videoWriter addr val vdev = (val, vdev & vidRAM %~ M.mWrite addr val)

-- | Make  a new video device at a given base address, adding it to an existing `MemorySystem`
mkVideoDevice :: (Num addrType,
                  Ord addrType,
                  ShowHex addrType,
                  DVU.Unbox wordType,
                  DeviceIO VideoDevice addrType wordType) =>
                 addrType
              -- ^ Video device's base address
              -> M.MemorySystem addrType wordType
              -- ^ Existing memory system into which video device is inserted
              -> (Int, M.MemorySystem addrType wordType)
              -- ^ Device index, updated memory system pair
mkVideoDevice base = M.mkDevRegion base (base + fromIntegral vidLinearSize) (mkDevice (mempty :: VideoDevice))
