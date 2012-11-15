{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Various and sundry utility functions
module Machine.Utils 
  ( ShowHex(..)
  , zeroFill
  , padTo
  , makeUpper
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import Numeric
import Data.Char
import Data.Word

-- | Type class that converts its output as leading zero, hexadecimal strings. This is fairly well
-- specialized to particular numeric types
class ShowHex x where
  asHex :: x -> ByteString                      -- ^ Convert value to 0-filled byte string (e.g, "034e" for 0x34e :: Word16)

  as0xHex :: x -> ByteString                    -- ^ Convert value to 0-filled byte string with "0x" prefix.
  as0xHex = (BC.append "0x") . asHex

  -- | 'asHex' converted to a String, useful in 'Show' instances
  asHexS :: x -> String
  asHexS = BC.unpack . asHex

  -- | 'as0xHex' converted to a String, useful in 'Show' instances
  as0xHexS :: x -> String
  as0xHexS = BC.unpack . as0xHex

instance ShowHex Word8 where
  asHex x = let s = showHex x ""
            in  BC.append (zeroFill 2 s) (BC.pack s)

instance ShowHex Word16 where
  asHex x = let s = showHex x ""
            in  BC.append (zeroFill 4 s) (BC.pack s)

instance (ShowHex x) => ShowHex [x] where
  asHex   x = BC.append "[" $ asHexList (asHex)   x BC.empty
  as0xHex x = BC.append "[" $ asHexList (as0xHex) x BC.empty

-- | Helper function for converting lists of things into hex
asHexList :: forall x. ShowHex x => (x -> ByteString)
          -> [x]
          -> ByteString
          -> ByteString
asHexList _asHexF [] s = BC.append "]" s
asHexList asHexF (x:[]) s = BC.append (asHexF x) (asHexList asHexF [] s)
asHexList asHexF (x:xs) s = BC.append (asHexF x) (BC.append ", " (asHexList asHexF xs s))

-- | Zero fill the front of a number, up to a given width
zeroFill :: Int
         -> String
         -> ByteString
zeroFill width s = let l = length s
                   in  if (l < width) then
                         BC.replicate (width - l) '0'
                       else
                         BC.empty

-- | Utility function that right pads the 'ByteString' with spaces up to a
-- given width. Nothing is done if the 'ByteString' is already longer than
-- the given width.
padTo :: Int                                    -- ^ Width
      -> ByteString                             -- ^ The incoming byte string
      -> ByteString                             -- ^ The resulting byte string
padTo width s = let l = BC.length s
                in  if (l < width) then
                      BC.append s (BC.replicate (width - l) ' ')
                    else
                      s

-- | Make a 'ByteString' all upper case.
makeUpper :: ByteString                         -- ^ Input 'ByteString'
          -> ByteString                         -- ^ Uppercased result
makeUpper = BC.map (\c -> if isLower c; then toUpper c; else c)