
module TRS80.CommonOptions
  ( CommonOptions(..)
  , mkCommonOptions
  , commonOptions
  , commonOptionUsage
  ) where

import           Data.Vector.Unboxed (Vector)
import           Data.Word
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Reader

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data CommonOptions where
  CommonOptions ::
    { imageReader :: FilePath -> IO (Vector Word8)
    , romPath     :: FilePath
    , memSize     :: Int
    } -> CommonOptions

mkCommonOptions :: CommonOptions
mkCommonOptions = CommonOptions
                { imageReader = readRawWord8Vector
                , romPath     = "no-such-path"
                , memSize     = 48
                }

commonOptions :: [OptDescr (CommonOptions -> IO CommonOptions)]
commonOptions =
 [ Option [] ["format"] (ReqArg setImageReader "<ROM image format>")   "ROM image reader ('ihex', 'intelhex' or 'raw')"
 , Option [] ["image"]  (ReqArg setROMImage    "ROM image file name")  "ROM image file name"
 , Option [] ["mem"]    (ReqArg setMemSize     "TRS-80's memory size") "Set the TRS-80's memory size (16/32/48)"
 ]
 where
  setImageReader fmt flags =
    case fmt of
      "ihex"     -> return $ flags { imageReader = readIntelHexVector }
      "intelhex" -> return $ flags { imageReader = readIntelHexVector }
      "raw"      -> return $ flags { imageReader = readRawWord8Vector }
      unknown    -> hPutStrLn stderr ("Unknown reader format: '" ++ (show unknown) ++ "'")
                    >> hPutStrLn stderr "Valid formats are 'ihex', 'intelhex' or 'raw'"
                    >> commonOptionUsage
                    >> exitFailure
                    >> return flags
  setROMImage fpath flags = return $ flags { romPath = fpath }
  setMemSize  msize flags = let msize' = (read msize :: Int)
                            in  if msize' `elem` [16, 32, 48]
                                then return $ flags { memSize = msize' }
                                else fail ("Invalid memory size: " ++ msize)

commonOptionUsage :: IO ()
commonOptionUsage = do
  prog <- getProgName
  hPutStrLn stderr ("Usage: " ++ prog ++ " <--format=(ihex|intelhex|raw)> image")
