{-# LANGUAGE RankNTypes #-}
-- | Various and sundry utility functions
module Machine.Utils where

import Text.Printf
import Data.Word

-- | Type class that converts its output as leading zero, hexadecimal strings. This is fairly well
-- specialized to particular numeric types
class ShowHex x where
  asHex :: x -> String

instance ShowHex Word8 where
  asHex x = printf "0x%02x" x

instance ShowHex Word16 where
  asHex x = printf "0x%04x" x

instance (ShowHex x) => ShowHex [x] where
  asHex x = "[" ++ (concat . asHexList $ x) ++ "]"

-- | Helper function for converting lists of things into hex
asHexList :: forall x. ShowHex x => [x]
          -> [String]
asHexList [] = []
asHexList (x:[]) = asHex x : []
asHexList (x:xs) = (asHex x) : ", " : (asHexList xs)