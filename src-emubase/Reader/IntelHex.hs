{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Intel Hex format parser/reader
module Reader.IntelHex
  ( readIntelHexVector
  ) where

-- import Debug.Trace

import           Control.Exception           hiding (try)
import           Control.Monad               (zipWithM_)
import           Control.Monad.Primitive     (PrimMonad (..))
import           Control.Monad.ST
import           Data.Bits                   (shiftR, xor, (.&.))
import           Data.Char                   (digitToInt)
import qualified Data.Foldable               as Foldable
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as Seq
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as DVU
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Word
import           Numeric
import           System.IO
import           Text.Parsec
import           Text.Parsec.Text

import           Reader.ErrorHandling

readIntelHexVector :: FilePath
                   -> IO (Vector Word8)
readIntelHexVector path =
  if (not . null) path then
    (TIO.readFile path
     >>= intelHexReader path) `catches` [ Handler (genericIOErrors "readIntelHexVector")
                                                            -- Add more handlers here, as needed
                                                            ]
  else
    hPutStrLn stderr "readIntelHexVector: Empty file name"
    >> invalidVector

-- | 'Parser' driver for reading the Intel hex-format byte string.
intelHexReader :: FilePath
               -> T.Text
               -> IO (Vector Word8)
intelHexReader path input =
  case parse intelHexParser path input of
    Left errMsg -> hPutStrLn stderr ("Error parsing Intel hex format: " ++ show errMsg)
                   >> invalidVector
    Right ihex  -> let ihex'                = (Seq.sort . trimEOF) ihex
                       maxAddr              = getLastAddr ihex'
                       tempVec              = DVU.create (M.new (fromIntegral maxAddr) >>= fillVector ihex')
                   in  return tempVec

-- | Fill the mutable vector from the sequence of 'IHexLine' records

-- Yes, we really do need this type signature here. And it took a lot of hacking from looking at
-- GHC's output to figure this out. Mutable vectors, as useful as they can be, are not easy to
-- work with.
fillVector :: Seq IHexLine                                      -- ^ Sequence of Intel hex-format data
           -> M.MVector s Word8                                 -- ^ Mutable vector being filled
           -> ST s (M.MVector (PrimState (ST s)) Word8)         -- ^ The result (because we have to return into a monad)
fillVector ihexSeq mvec = Foldable.mapM_ (processLine mvec) ihexSeq
                          >> return mvec
  where
    -- Create a list of indices that are zipped with the actual byte data, and write them into the mutable vector.
    processLine vec line =
     let sAddr = (fromIntegral . addr) line
         eAddr = sAddr + (length . bytes) line
     in  zipWithM_ (M.write vec) [sAddr..eAddr] (bytes line)

-- | The data contents of a type '0' and type '1' records. Type '0' is an address and associated data. Type '1' is an end-of-file
-- record, in which the address can be used to specify where execution starts.
data IHexLine =
    IHexType0
    { addr  :: Word16
    , bytes :: [Word8]
    }
  | IHexType1
    { startAddr :: Word16
    }
  deriving (Show)

-- | Instance of 'Eq' for 'IHexLine'
instance Eq IHexLine where
  a@(IHexType0 {}) == b@(IHexType0 {})   = addr a == addr b && bytes a == bytes b
  (IHexType1 {})   == (IHexType1 {})     = True
  _a               == _b                 = False

-- | Instance of 'Ord' for 'IHexLine'
instance Ord IHexLine where
  -- IHexType0 orders by address
  compare a@(IHexType0 {}) b@(IHexType0 {}) = compare (addr a) (addr b)
  -- IHexType1 should always compare greater than anything else
  compare (IHexType1 {})   (IHexType1{})    = EQ
  compare (IHexType1 {})   _right           = GT
  compare _left            (IHexType1 {})   = LT

-- | Type '0' end of file data record
type0eof :: IHexLine
type0eof = IHexType0
           { addr = 0
           , bytes = []
           }

-- | Type '1' end of file data record
type1eof :: IHexLine
type1eof = IHexType1 0x0

-- | The actual Intel hex-format parser: read data line, ended by a newline. Wash. Rinse. Repeat. Convert to 'Seq' sequence
-- when complete.
intelHexParser :: Parser (Seq IHexLine)
intelHexParser = fmap Seq.fromList (readIHexLine `endBy` newline)

-- | Parse a line of Intel hex input.
readIHexLine :: Parser IHexLine
readIHexLine =
  (string ":" <?> "Missing ':' at start of line")
  *> do
      r_nBytes  <- readByte <?> "byte count"
      r_addr    <- readAddr <?> "16-bit address"
      r_recType <- readByte <?> "record type"
      r_bytes   <- count (fromIntegral r_nBytes) readByte <?> "data sequence"
      csum      <- readByte <?> "checksum"

      let seenCSum = computedCheckSum r_addr r_recType r_bytes
      if seenCSum == csum then
        case r_recType of
          0          -> return $ IHexType0 { addr = r_addr, bytes = r_bytes }
          1          -> return $ IHexType1 { startAddr = r_addr }
          _otherwise -> fail ("Extended record type " ++ show r_recType ++ " not supported.")
      else
        fail ("Invalid checksum: Got 0x" ++ showHex csum "" ++ ", expecting 0x" ++ showHex seenCSum "")

-- | Compute the checksum over all of the read fields. The checksum is the 2's complement sum of the data bytes,
-- record type, data byte count and the address bytes
computedCheckSum :: Word16
                 -> Word8
                 -> [Word8]
                 -> Word8
computedCheckSum theAddr rType theBytes =
  ((sum theBytes
   + (fromIntegral . length) theBytes
   + rType
   + fromIntegral (theAddr `shiftR` 8)
   + fromIntegral (theAddr .&. 0xff)) `xor` 0xff) + 1

-- | Parse a byte (two hex digits) from the 'Parser' stream, returning the byte\'s value.
readByte :: Parser Word8
readByte =
  hexDigit
  >>= (\hi -> hexDigit
              >>= (\lo -> (return . fromIntegral) $ digitToInt hi * 16 + digitToInt lo))

-- | Parse a big-endian 16-bit address from the 'Parser' stream, returning the address\' value.
readAddr :: Parser Word16
readAddr = do
  hiAddr <- readByte
  loAddr <- readByte
  return (fromIntegral hiAddr * 256 + fromIntegral loAddr)

-- | Trim end-of-file records off the end of the sequence, if present
trimEOF :: Seq IHexLine
        -> Seq IHexLine
trimEOF ihex =
  let lastRec = ihex `Seq.index` (Seq.length ihex - 1)
  in  if lastRec == type0eof || lastRec == type1eof then
        Seq.take (Seq.length ihex - 1) ihex
      else
        ihex

-- | Get address + length of the last data record as the maximum address of the
getLastAddr :: Seq IHexLine
            -> Word16
getLastAddr ihex = let lastRec = ihex `Seq.index` (Seq.length ihex - 1)
                   in  case lastRec of
                         a@(IHexType0 {}) -> addr a + (fromIntegral . length . bytes) a
                         _otherwise       -> error "Reader.IntelHex.getLastAddr -- last record not IHexType0?"
