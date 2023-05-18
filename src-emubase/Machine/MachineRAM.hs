{-  # LANGUAGE AllowAmbiguousTypes       #  -}
{-  # LANGUAGE ExistentialQuantification #  -}
{-  # LANGUAGE FlexibleInstances         #  -}
{-# LANGUAGE GADTs                     #-}
{-  # LANGUAGE RankNTypes                #  -}
{-  # LANGUAGE ScopedTypeVariables       #  -}
{-# LANGUAGE TypeFamilies              #-}
{-  # LANGUAGE UndecidableInstances      #  -}

{- | Machine Random Access Memory
-}

module Machine.MachineRAM 
  ( MachineRAM
  , Addr8ByteRAM
  , Addr16ByteRAM
  , Addr32ByteRAM
  , Addr16WordRAM
  , mkAddr16ByteRAM
  , byteRAMContent
  , testRAM
  )
where

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Typeable          (Typeable)
import qualified Data.Vector.Unboxed    as DVU
import           Data.Word              (Word32, Word16, Word8)

import           Machine.Device

-- | Device type tag for random access memory devices
data MachineRAM
  deriving (Eq, Show)

-- | RAM with a parameterized address type and bytes as the 'wordType' unit.
instance
    ( Integral addrType
    )
  => DeviceIO MachineRAM addrType Word8 where
  newtype instance Device MachineRAM addrType Word8 =
    ByteRAM {
      byteRAMContent :: DVU.Vector Word8
    }
    deriving (Eq, Show, Typeable)
  doDevRead = ramReader
  doDevWrite = ramWriter

-- | 8-bit address byte RAM
type Addr8ByteRAM  = Device MachineRAM Word8 Word8
-- | 16-bit address byte RAM (e.g. Zilog Z80 and Motorola 6502)
type Addr16ByteRAM = Device MachineRAM Word16 Word8
-- | 32-bit address byte RAM
type Addr32ByteRAM = Device MachineRAM Word32 Word8
-- | 16-bit address word RAM (e.g., DEC PDP-11)
type Addr16WordRAM = Device MachineRAM Word16 Word16

-- | Read a segment of RAM, returning the original RAM device and vector slice. RAM doesn't change during
-- a read (duh!). The vector slice is zero-padded if the number of 'wordType' units to read is longer than
-- the 'byteRAMContent' vector's length.
ramReader ::
    ( MonadIO m
    , Integral addrType
    )
  => addrType
  -> Int
  -> Device MachineRAM addrType Word8
  -> m (Device MachineRAM addrType Word8, DVU.Vector Word8)
ramReader startOffs nRead ramDev = do
  pure (ramDev, DVU.concat [DVU.slice startOffs' sliceLen ramVector, zeroFill])
  where
    ramVector = byteRAMContent ramDev
    startOffs' = fromIntegral startOffs
    sliceLen  =
      if   startOffs' + nRead < DVU.length ramVector
      then nRead
      else DVU.length ramVector - startOffs'
    zeroFill =
      if sliceLen == nRead
      then DVU.empty
      else DVU.replicate (nRead - sliceLen) 0

ramWriter ::
    (MonadIO m)
  => addrType
  -> DVU.Vector wordType
  -> Device MachineRAM addrType Word8
  -> m (Device MachineRAM addrType Word8)
ramWriter _addr _devData ramDev =  do
  pure ramDev

mkAddr16ByteRAM :: Word16 -> Word16 -> Addr16ByteRAM
mkAddr16ByteRAM startAddr endAddr = ByteRAM { byteRAMContent = DVU.replicate (fromIntegral (startAddr - endAddr)) 0 }

testRAM :: IO ()
testRAM = do
    putStrLn "testRAM: calling doDevRead"
    (dev', thing) <- readRAM
    print thing
    print dev'
    putStrLn "doDevRead done!"

    putStrLn "testRAM: calling deviceRead"
    (dev_effect, thing_effect) <- runDeviceReadC readRAM'
    print thing_effect
    print dev_effect
    putStrLn "deviceRead done!"

    putStrLn "testRAM, calling doDevWrite"
    dev'' <- writeRAM
    print dev''
    putStrLn "doDevWrite done!"

    putStrLn "testRAM, calling deviceWrite"
    dev_write <- runDeviceWriteC writeRAM'
    print dev_write
    putStrLn "deviceWrite done!"
    where
      ramDevice = mkAddr16ByteRAM 0 0xffff
      readRAM =  doDevRead (0 :: Word16) 1 ramDevice
      readRAM' = deviceRead (0 :: Word16) 1 ramDevice
        :: DeviceReadC Word16 Word8 MachineRAM IO (Device MachineRAM Word16 Word8, DVU.Vector Word8)
      writeRAM = doDevWrite (0 :: Word16) (DVU.empty :: DVU.Vector Word8) ramDevice
      writeRAM' = deviceWrite (0 :: Word16) (DVU.empty :: DVU.Vector Word8) ramDevice
        :: DeviceWriteC Word16 Word8 MachineRAM IO (Device MachineRAM Word16 Word8)
