{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{- | Test devices for memory system testing.
-}
module Machine.Tests.TestDevice
        ( mkTestDevice
        , mkVideoDevice
        , vidRows
        , vidCols
        , vidLinearSize
        , videoTestPattern
        ) where

import           Control.Arrow              (second)
import           Control.Lens               (Lens', set, (%~), (&), (^.))
import           Control.Monad.State.Strict (state)
import           Data.Char                  (ord)
import           Data.List                  (cycle)
import qualified Data.Vector.Unboxed        as DVU
import           Data.Word

import           Machine.Device
import qualified Machine.MemorySystem       as M
import           Machine.Types              (Device, DeviceFuncs (..), DeviceIO (..), EmulatedSystem, SimpleDeviceSystem,
                                             mkSimpleDeviceSystem)
import           Machine.Utils


-- | A very simple counter device
newtype TestDevice = TestDevice Int
  deriving (Show)

instance Monoid TestDevice where
  -- Use non-zero starting value (since a memory system read will return 0 if an address cannot be read or isn't in a
  -- mapped region.)
  mempty = TestDevice 19
  (TestDevice a) `mappend` (TestDevice b) = TestDevice (a + b)

-- Instantiate the DeviceFuncs type class:
instance DeviceFuncs TestDevice

-- Instantiate the DeviceIO class
instance (Integral wordType) => DeviceIO TestDevice addrType wordType where
  readDeviceWord _addr       = state testDeviceReader
  writeDeviceWord _addr _word = state (\s -> ((), s))

-- | Example device reader function that just increments the counter as the updated state. Slightly less overhead
-- than using MonadState 'get' and 'put'. The address is ignored.
--
-- An alternative implementation that uses MonadState 'get' and 'put':
--
-- > instance (Integral wordType) => DeviceIO TestDevice addrType wordType where
-- >   readDeviceWord = testDeviceReader
-- >
-- > testDeviceReader :: (Integral wordType) => DevReaderFunc TestDevice addrType wordType
-- >testDeviceReader _addr = get >>= (\x -> put (x + 1) >> return (fromIntegral x))
--
testDeviceReader :: (Integral wordType) =>
                    TestDevice
                 -> (wordType, TestDevice)
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
    { _vidRAM   :: VideoRAMSystem
    } -> VideoDevice
  deriving (Show)

-- | Phantom type tag for this video RAM device/system
data VideoRAM

-- | The video system
type VideoRAMSystem = SimpleDeviceSystem VideoRAM Word16 Word8

-- | Getter/setter lens for _vidRAM
vidRAM :: Lens' VideoDevice VideoRAMSystem
vidRAM f vdev = (\vram -> vdev { _vidRAM = vram}) <$> f (_vidRAM vdev)

-- | Test pattern for video device.
videoTestPattern :: [Word8]
videoTestPattern = map (fromIntegral . ord) $ take vidLinearSize (cycle (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']))

-- | The `Monoid` instance
instance Monoid VideoDevice where
  mempty           = VideoDevice {
                      _vidRAM   = M.mkRAMRegion 0 vidLinearSize (mkSimpleDeviceSystem "VRAM")
                     } & vidRAM %~ M.mPatch 0 (DVU.fromList videoTestPattern)
  _ `mappend` vidB = vidB

-- | Default instance for `DeviceFuncs`
instance DeviceFuncs VideoDevice

-- | `DeviceIO` for the video memory
instance DeviceIO VideoDevice Word16 Word8 where
  readDeviceWord       = state . videoReader
  writeDeviceWord addr = state . videoWriter addr

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
            -> ((), VideoDevice)
videoWriter addr val vdev = ((), vdev & vidRAM %~ M.mWrite addr val)

-- | Make  a new video device at a given base address, adding it to an existing `MemorySystem`
mkVideoDevice :: (Num addrType,
                  Ord addrType,
                  ShowHex addrType,
                  DVU.Unbox wordType,
                  DeviceIO VideoDevice addrType wordType) =>
                 addrType
              -- ^ Video device's base address
              -> EmulatedSystem cpuType insnSet addrType wordType
              -- ^ System to which the video device is being added
              -> (Int, EmulatedSystem cpuType insnSet addrType wordType)
              -- ^ Device index, updated system pair
mkVideoDevice base = M.mkDevRegion base (base + fromIntegral vidLinearSize) (mkDevice (mempty :: VideoDevice))
