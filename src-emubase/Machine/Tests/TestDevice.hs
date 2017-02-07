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

import           Control.Arrow               (second)
import           Lens.Micro.Platform         (Lens', set, (^.), (%~), (&))
import           Data.Char                   (ord)
import           Data.List                   (cycle)
import qualified Data.Vector.Unboxed         as DVU
import           Data.Word
import           Data.Semigroup()

import qualified Machine.MemorySystem        as M
import qualified Machine.Device              as D

#if defined(TEST_DEBUG)
import           Debug.Trace
import           Machine.Utils
#endif

-- | A very simple counting device
newtype TestDevice = TestDevice Int
  deriving (Show)

testDevReset :: D.DeviceReset Word16 Word8 TestDevice
testDevReset  _dev                   = TestDevice 19
testDevReader :: D.DeviceReader Word16 Word8 TestDevice
testDevReader _addr (TestDevice cnt) = (fromIntegral cnt, TestDevice (1+ cnt))
testDevWriter :: D.DeviceWriter Word16 Word8 TestDevice
testDevWriter _addr _word testDev    = ((), testDev)

-- | And finally, a factory constructor function.
mkTestDevice :: D.Device Word16 Word8
mkTestDevice = D.Device (TestDevice 19) testDevReset testDevReader testDevWriter

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

instance Semigroup VideoDevice where
  _ <> vidB = vidB

-- | The `Monoid` instance
instance Monoid VideoDevice where
  mempty           = VideoDevice {
                      _vidRAM   = M.mkRAMRegion 0 vidLinearSize mempty
                     } & vidRAM %~ M.mPatch 0 (DVU.fromList videoTestPattern)
  mappend = (<>)

-- | The video system
type VideoRAMSystem = M.MemorySystem Word16 Word8

-- | Getter/setter lens for _vidRAM
vidRAM :: Lens' VideoDevice VideoRAMSystem
vidRAM f vdev = (\vram -> vdev { _vidRAM = vram}) <$> f (_vidRAM vdev)

-- | Test pattern for video device.
videoTestPattern :: [Word8]
videoTestPattern = map (fromIntegral . ord) $ take vidLinearSize (cycle (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']))

-- | Number of rows for this video device
vidRows :: Int
vidRows = 24
--- | Number of columns for this video device
vidCols :: Int
vidCols = 64
-- | Linear size of the video RAM
vidLinearSize :: Int
vidLinearSize = vidRows * vidCols

-- | Reset the video device
videoReset :: D.DeviceReset Word16 Word8 VideoDevice
videoReset _dev = mempty :: VideoDevice

-- | Read a byte from the video device's memory
videoReader :: D.DeviceReader Word16 Word8 VideoDevice
videoReader addr vdev = second updVidRAM (M.mRead addr (vdev ^. vidRAM))
  where
    updVidRAM vram = set vidRAM vram vdev

-- | Write a byte to the video device's memory
videoWriter :: D.DeviceWriter Word16 Word8 VideoDevice
videoWriter val addr vdev = ((), vdev & vidRAM %~ M.mWrite addr val)

-- | Make  a new video device at a given base address, adding it to an existing `MemorySystem`
mkVideoDevice :: Word16
              -> M.MemorySystem Word16 Word8
              -> M.MemorySystem Word16 Word8
mkVideoDevice baseAddr = M.mkDevRegion baseAddr vidLinearSize videoDev
  where
    videoDev = D.Device (mempty :: VideoDevice) videoReset videoReader videoWriter
