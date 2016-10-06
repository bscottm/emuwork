-- | Raw format reader
module Reader.RawFormat
  ( -- * Functions
    readRawWord8Vector
  ) where

import           Control.Exception
import           Control.Monad               (zipWithM_)
import qualified Data.ByteString.Lazy        as BL
import           System.IO

import qualified Data.Vector.Unboxed         as DVU
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word

import           Reader.ErrorHandling

-- | Read a raw 'Word8' vector from a file.
--
-- Note 1: This is non-polymorphic: It just reads an unboxed vector from a file.
-- If further transformations are needed, they need to be applied afterward.
--
-- Note 2: If an error occurs while reading the file, the resulting vector is an empty
-- (null) vector.
readRawWord8Vector :: FilePath                  -- ^ File to read
                   -> IO (DVU.Vector Word8)     -- ^ The resulting vector.
readRawWord8Vector path =
  if (not . null) path then
    (BL.readFile path >>= fillVector) `catches` [ Handler (genericIOErrors "readRawWord8Vector")
                                                -- Add more handlers here, as needed
                                                ]
  else
    hPutStrLn stderr "readRawWord8Vector: Empty file name"
    >> invalidVector
  where
    vecLen contents               = fromIntegral (BL.length contents)
    fillVector contents           = return $ DVU.create (M.new (vecLen contents) >>= unpackAndFillVec contents)
    unpackAndFillVec contents vec = zipWithM_ (M.write vec)
                                              [(0 :: Int)..(vecLen contents)]
                                              (BL.unpack contents)
                                    >> (vec `seq` return vec)
