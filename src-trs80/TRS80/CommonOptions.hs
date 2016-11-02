{-# OPTIONS_HADDOCK ignore-exports #-}

{- | TRS-80 common option processing.

Collect the common options used between all of the emulator commands required to configure
a working TRS-80 Model I system.

- Image reader/format: "raw", "intelhex", "hex"
- Memory size (16/32/48K)
-}

module TRS80.CommonOptions
  ( CommonOptions(..)
  , getCommonOptions
  , commonOptionUsage
  ) where

import qualified Data.Foldable as Foldable
import           Data.Vector.Unboxed (Vector)
import           Data.Word
import           System.Console.GetOpt
import           System.IO

import           Reader

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data CommonOptions where
  -- | Collected options to configure a TRS-80 Model I system. This is normally what gets
  -- returned.
  CommonOptions ::
    { imageReader :: (FilePath -> IO (Vector Word8))
    , romPath     :: FilePath
    , memSize     :: Int
    } -> CommonOptions

  -- | Internal intermediate error: Invalid reader format
  InvalidReader  :: String
                 -> CommonOptions

  -- | Internal intermediate error: Invalid memory size
  InvalidMemSize :: String
                 -> CommonOptions

  -- | Detected an invalid option along the way... this gets returned to the caller.
  InvalidOptions :: CommonOptions

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

getCommonOptions :: [String] -> IO (CommonOptions, [String], [String])
getCommonOptions opts =
  case getOpt' RequireOrder commonOptions opts of
    (optsActions, rest, unOpts, [])   -> validateOptions (Foldable.foldl' (flip id) mkCommonOptions optsActions)
                                         >>= (\opts' -> return (opts', rest, unOpts))
    (_,           _,    _,      errs) -> hPutStrLn stderr "TRS-80 common options:"
                                         >> mapM_ (hPutStrLn stderr) errs
                                         >> commonOptionUsage
                                         >> return (InvalidOptions, [], [])
                
mkCommonOptions :: CommonOptions
mkCommonOptions = CommonOptions
                { imageReader = readRawWord8Vector
                , romPath     = ""
                , memSize     = 48
                }

commonOptions :: [OptDescr (CommonOptions -> CommonOptions)]
commonOptions =
 [ Option [] ["format"] (ReqArg setImageReader "FMT")        "ROM image reader ('ihex', 'intelhex' or 'raw')"
 , Option [] ["image"]  (ReqArg setROMImage    "FILE")       "ROM image file name"
 , Option [] ["mem"]    (ReqArg setMemSize     "[16|32|48]") "Set the TRS-80's memory size (16/32/48)"
 ]
 where
  setImageReader fmt flags =
    case fmt of
      "ihex"     -> flags { imageReader = readIntelHexVector }
      "intelhex" -> flags { imageReader = readIntelHexVector }
      "raw"      -> flags { imageReader = readRawWord8Vector }
      unknown    -> InvalidReader unknown

  setROMImage fpath flags = flags { romPath = fpath }
  setMemSize  msize flags = let msize' = (read msize :: Int)
                            in  if msize' `elem` [16, 32, 48]
                                then flags { memSize = msize' }
                                else InvalidMemSize msize

validateOptions :: CommonOptions
                -> IO CommonOptions
validateOptions (InvalidReader rdr) =
  (hPutStr stderr $ unlines [ ("Invalid ROM reader format: " ++ rdr)
                            , "Valid formats are 'ihex', 'intelhex' or 'raw'"
                            , ""
                            ])
  >> commonOptionUsage
  >> return InvalidOptions
validateOptions InvalidOptions =
  hPutStrLn stderr "InvalidOptions while validating options?"
  >> return InvalidOptions
validateOptions (InvalidMemSize msize) =
  (hPutStr stderr $ unlines [ "Invalid memory size (" ++ msize ++ "), expected 16, 32 or 48"
                            , ""
                            ])
  >> commonOptionUsage
  >> return InvalidOptions
validateOptions opts
  | null (romPath opts)
  = (hPutStr stderr $ unlines [ "TRS-80 ROM file name missing."
                              , ""
                              ])
    >> commonOptionUsage
    >> return InvalidOptions
  | otherwise
  = return opts

commonOptionUsage :: IO ()
commonOptionUsage = do
  hPutStrLn stderr (usageInfo "Common options:" commonOptions)
