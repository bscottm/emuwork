{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Machine.MachineRAM 
  ( MachineRAM
  , MachineByteRAM
  , testRAM
  )
where

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Typeable          (Typeable)
import qualified Data.Vector.Unboxed    as DVU
import           Data.Word              (Word16, Word8)

import           Machine.Device


data MachineRAM
newtype instance Device MachineRAM addrType Word8 =
  ByteRAM {
    byteRAMContent :: DVU.Vector Word8
  }
  deriving (Eq, Show, Typeable)

type MachineByteRAM addrType = Device MachineRAM addrType Word8

ramReader ::
  (Integral addrType, MonadIO m) =>
  addrType
  -> Int
  -> Device MachineRAM addrType Word8
  -> m (Device MachineRAM addrType Word8, DVU.Vector Word8)
ramReader addr nRead ramDev = do
      liftIO $ putStrLn "MachineRAM general read."
      pure (ramDev, DVU.concat [DVU.slice startOffs sliceLen ramVector, zeroFill])
      where
        ramVector = byteRAMContent ramDev
        startOffs = fromIntegral addr
        sliceLen  =
          if   startOffs + nRead < DVU.length ramVector
          then nRead
          else DVU.length ramVector - startOffs
        zeroFill =
          if sliceLen == nRead
          then DVU.empty
          else DVU.replicate (nRead - sliceLen) 0

ramWriter ::
    (MonadIO m) =>
  addrType ->
  DVU.Vector wordType ->
  Device MachineRAM addrType Word8 ->
  m (Device MachineRAM addrType Word8)
ramWriter _addr _devData ramDev =  do
      liftIO $ putStrLn "MachineRAM doDevWrite"
      pure ramDev

instance DeviceIO MachineRAM Word16 Word8 where
  doDevRead = ramReader
  doDevWrite = ramWriter


testRAM :: IO ()
testRAM = do
    putStrLn "testRAM, calling doDevRead"
    (dev', thing) <- runDeviceReadC readRAM
    print thing
    print dev'
    putStrLn "doDevRead done!"
    putStrLn "testRAM, calling doDevWrite"
    dev'' <- runDeviceWriteC writeRAM
    print dev''
    putStrLn "doDevWrite done!"
    where
        ramDevice = ByteRAM { byteRAMContent = DVU.empty } -- :: Device MachineRAM Word16 Word8
        readRAM =  deviceRead (0 :: Word16) 1 ramDevice 
          -- :: DeviceReadC Word16 Word8 (MachineByteRAM Word16) IO (MachineByteRAM Word16, DVU.Vector Word8)
        writeRAM = deviceWrite (0 :: Word16) (DVU.empty :: DVU.Vector Word8) ramDevice
        --   :: DeviceWriteC Word16 Word8 (MachineByteRAM Word16) IO (MachineByteRAM Word16)
