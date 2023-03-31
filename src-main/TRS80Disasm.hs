module Main (main) where

import           Control.Monad

import qualified Data.ByteString       as S
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe)
import           Data.Vector.Unboxed   (Vector)
import           Data.Word
import qualified Data.Yaml             as Y
import qualified Data.Text as T

import           Reader                (readIntelHexVector, readRawWord8Vector)

import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder), OptDescr (..), getOpt, usageInfo)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import           System.IO             (hPutStr, hPutStrLn, stderr)

import           TRS80                 (trs80disassemble, trs80generic, yamlFileGuidance)

main :: IO ()
main =
  (getArgs >>= parseArgs . getOpt RequireOrder disasmOptions)
    >>= runIt 
  where
    -- We get a triple, not three separate arguments:
    parseArgs (opts, rest, errs) =
      do
        unless (null errs)
          (mapM_ (hPutStrLn stderr) errs
           >> showUsage
           >> exitFailure)
        -- Combine the options together into DisasmArgs
        opts <- foldM (flip ($)) mkDisasmArgs opts
        good <- sanityCheck opts rest
        unless good
          (do
            showUsage
            exitFailure
          )
        return $ setRemaining opts rest

    sanityCheck opts rest
      | showParsed opts && disasmCmd opts /= CheckGuidance
      = do
          hPutStrLn stderr "--verbose only applies to checking a guidance YAML file."
          return False
      | disasmCmd opts == CheckGuidance && length rest /= 1
      = do
          hPutStrLn stderr "--check requires a guidance YAML file."
          return False
      | disasmCmd opts == Disassemble && length rest /= 2
      = do
          hPutStrLn stderr "disassembly requires an image file and a guidance YAML file."
          return False
      | otherwise
      = return True

    setRemaining opts rest
      | disasmCmd opts == CheckGuidance
      = opts { guidanceFile = head rest }
      | disasmCmd opts == Disassemble
      = let (img:gFile:_) = take 2 rest
        in opts { imageFile = img, guidanceFile = gFile }
    
    runIt :: DisasmArgs -> IO ()
    runIt opts
      | disasmCmd opts == CheckGuidance
      = do
          yamlResult <- yamlFileGuidance . guidanceFile $ opts
          let gotError      = putStrLn . Y.prettyPrintParseException
              gotResult     = when (showParsed opts) . S.putStr . Y.encode
          either gotError gotResult yamlResult
      | disasmCmd opts == Disassemble
      = do
          let gFile = guidanceFile opts
          gresult <- yamlFileGuidance gFile
          let gotError err = hPutStrLn stderr (intercalate "\n" [ gFile ++ ":", Y.prettyPrintParseException err ])
              gotResult    = trs80disassemble trs80generic (fromMaybe readRawWord8Vector (imageReader opts)) (imageFile opts) 48
          either gotError gotResult gresult
          return ()

showUsage :: IO ()
showUsage = do
  prog <- T.pack <$> getProgName
  let prgName = fromMaybe (fromMaybe prog $ T.stripSuffix ".exe" prog) $ T.stripSuffix ".EXE" prog
  let header = T.concat ["Usage: ", prgName, " [OPTIONS]"]
  hPutStrLn stderr (usageInfo (T.unpack header) disasmOptions)
  exitFailure

data DisasmCmd = CheckGuidance | Disassemble
  deriving Eq

data DisasmArgs where
  DisasmArgs ::
    { showParsed     :: Bool
    , disasmCmd  :: DisasmCmd
    , imageReader    :: Maybe (FilePath -> IO (Vector Word8))
    , imageFile      :: FilePath
    , guidanceFile   :: FilePath
    } -> DisasmArgs

mkDisasmArgs :: DisasmArgs
mkDisasmArgs =
  DisasmArgs
  { showParsed     = False
  , disasmCmd  = Disassemble
  , imageReader    = Nothing
  , imageFile      = ""
  , guidanceFile   = ""
  }

disasmOptions :: [OptDescr (DisasmArgs -> IO DisasmArgs)]
disasmOptions =
  [ Option ['v'] ["verbose"] (NoArg setParsed) "Output the parsed disassembly guidance."
  , Option ['c'] ["check"]   (NoArg setCheckGuidance) "Check the guidance file"
  , Option []    ["format"]  (ReqArg setImageReader "FMT") "ROM image reader ('ihex', 'intelhex' or 'raw')"
  ]
  where
    setParsed arg = return $ arg { showParsed = True }
    setCheckGuidance arg = return $ arg { disasmCmd = CheckGuidance }
    setImageReader fmt flags@DisasmArgs{} =
      case fmt of
        "ihex"     -> return flags { imageReader = Just readIntelHexVector }
        "intelhex" -> return flags { imageReader = Just readIntelHexVector }
        "raw"      -> return flags { imageReader = Just readRawWord8Vector }
        rdr        -> hPutStr stderr (unlines [ "Invalid ROM reader format: " ++ rdr
                                              , "Valid formats are 'ihex', 'intelhex' or 'raw'"
                                              , ""
                                              ])
                      >> showUsage
                      >> exitFailure