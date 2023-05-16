module Main where

import Control.Algebra ()

import           Machine.Device

main :: IO ()
main = do
    putStrLn "Running!"
    (_conDev, readData) <- devRead 0 1 RAMDevice
    print readData
    putStrLn "Done!"