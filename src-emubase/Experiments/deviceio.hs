module Main where

import           Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Data.Vector.Unboxed    as DVU
import Data.Char (ord, chr)
import           Data.Word              (Word16, Word8)

import           Machine.Device         (DeviceIO (..), DeviceT (..))

type FooAddr = Word16
type FooWord = Word8

data FooDevice1 = FooDevice1

instance Show FooDevice1 where
    show FooDevice1 = "FooDevice1"

type FooDevice_1 = DeviceT FooAddr FooWord IO FooDevice1

fooDevice :: FooDevice_1
fooDevice = DeviceT (pure FooDevice1)

instance DeviceIO FooAddr FooWord IO FooDevice1 where
    deviceWrite devData _addr dev = do
        fooDev <- dev
        _ <- liftIO $ foldMap (putChar . chr . fromIntegral) (DVU.toList devData)
        return fooDev

main :: IO ()
main = do
    putStrLn "Running!"
    wombat <- theDevice $ deviceWrite (DVU.fromList $ map (fromIntegral . ord) "Hello world!\n") 0x3000 fooDevice
    print wombat
    putStrLn "Done!"