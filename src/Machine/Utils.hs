-- | Various and sundry utility functions
module Machine.Utils 
  ( -- * Types and classes
    ShowHex(..)
  , SignExtend(..)

    -- * Functions
  , zeroFill
  , padTo
  , makeUpper
  , textSpace
  , textZero
  ) where

import Data.Int
import Data.Bits
import qualified Data.Text as T
import Numeric
import Data.Char
import Data.Word

-- | Type class that converts its output as leading zero, hexadecimal strings. This is fairly well
-- specialized to particular numeric types
class ShowHex x where
  asHex :: x -> T.Text          -- ^ Convert value to 0-filled byte string (e.g, "034e" for 0x34e :: Word16)

  as0xHex :: x -> T.Text        -- ^ Convert value to 0-filled byte string with "0x" prefix.
  as0xHex = (T.append "0x") . asHex

  -- | 'asHex' converted to a String, useful in 'Show' instances
  asHexS :: x -> String
  asHexS = T.unpack . asHex

  -- | 'as0xHex' converted to a String, useful in 'Show' instances
  as0xHexS :: x -> String
  as0xHexS = T.unpack . as0xHex

instance ShowHex Word8 where
  asHex x = let s = T.pack $ showHex x ""
            in  T.justifyRight 2 '0' s

instance ShowHex Word16 where
  asHex x = let s = T.pack $ showHex x ""
            in  T.justifyRight 4 '0' s

instance (ShowHex x) => ShowHex [x] where
  asHex   x = T.append "[" $ asHexList (asHex)   x T.empty
  as0xHex x = T.append "[" $ asHexList (as0xHex) x T.empty

-- | Helper function for converting lists of things into hex
asHexList :: ShowHex x => (x -> T.Text)
          -> [x]
          -> T.Text
          -> T.Text
asHexList _asHexF [] s = T.append "]" s
asHexList asHexF (x:[]) s = T.append (asHexF x) (asHexList asHexF [] s)
asHexList asHexF (x:xs) s = T.append (asHexF x) (T.append ", " (asHexList asHexF xs s))

-- | Zero fill the front of a number, up to a given width
zeroFill :: Int
         -> T.Text
         -> T.Text
zeroFill width s = let l = T.length s
                   in  if (l < width) then
                         T.replicate (width - l) textZero
                       else
                         T.empty

-- | Utility function that right pads the 'T.Text' with spaces up to a
-- given width. Nothing is done if the 'T.Text' is already longer than
-- the given width.
padTo :: Int            -- ^ Width
      -> T.Text         -- ^ The incoming byte string
      -> T.Text         -- ^ The resulting byte string
padTo width s = T.justifyLeft width ' ' s

-- | Make a 'T.Text' string all upper case.
makeUpper :: T.Text                             -- ^ Input 'T.Text'
          -> T.Text                             -- ^ Uppercased result
makeUpper = T.map (\c -> if isLower c; then toUpper c; else c)

-- | 'T.Text' space character (since it's used so much.)
textSpace :: T.Text
textSpace = T.singleton ' '

-- | 'T.Text' zero character
textZero :: T.Text
textZero = T.singleton '0'

-- | Sign extension type class: generally useful for conversions to 'Int32' or 'Int' when having to manipulate unsigned
-- and signed types. GHC makes no formal guarantees on sign extension when using 'fromIntegral'. Minimum implementation
-- is 'signExtend64', which can be truncated down to 'Int32'.
class SignExtend wordType where
  signExtend :: wordType
             -> Int32
  signExtend64 :: wordType
                -> Int64

  -- Default implementation is to truncate the 64-bit sign extension
  signExtend = fromIntegral . signExtend64

instance SignExtend Word16 where
  signExtend64 x = let x' = fromIntegral x :: Int64
                   in  if x <= 0x7fff then
                         x'
                       else
                         x' .|. (complement 0xffff)

instance SignExtend Int16 where
  signExtend64 x = let x' = fromIntegral x :: Int64
                   in  if x <= 0x7fff then
                         x'
                       else
                         -((x' `xor` 0xffff) + 1)
