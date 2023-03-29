module Main (main) where

import           Control.Monad

import qualified Data.ByteString       as S
import qualified Data.Foldable         as Foldable
import qualified Data.Yaml             as Y

import           System.Console.GetOpt (ArgOrder (RequireOrder), getOpt, OptDescr (..), ArgDescr (..))
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import           System.IO             (hPutStrLn, stderr)

import           TRS80.Disasm.Guidance

main :: IO ()
main =
  (getArgs >>= combineArgs . getOpt RequireOrder disasmOptions)
    >>= runIt 
  where
    -- We get a triple, not three separate arguments:
    combineArgs (opts, rest, errs) =
      do
        unless (null errs || length rest /= 1) (mapM_ (hPutStrLn stderr) errs >> showUsage >> exitFailure)
        newopts <- Foldable.foldl (>>=) (return mkDisasmCheckArgs) opts
        return $ newopts { files = rest }
    
    runIt :: DisasmCheckArgs -> IO ()
    runIt opts = do
      yamlResult <- yamlFileGuidance (head . files $ opts)
      let gotError      = putStrLn . Y.prettyPrintParseException
          gotResult     = when (showParsed opts) . S.putStr . Y.encode
      either gotError gotResult yamlResult

showUsage :: IO ()
showUsage = do
  prog <- getProgName
  hPutStrLn stderr ("Usage: " ++ prog ++ " [--show-content] [--show-result]")
  exitFailure

data DisasmCheckArgs =
  DisasmCheckArgs
  { showParsed :: Bool
  , files :: [FilePath]
  }

mkDisasmCheckArgs :: DisasmCheckArgs
mkDisasmCheckArgs =
  DisasmCheckArgs
  { showParsed = False
  , files = []
  }

disasmOptions :: [OptDescr (DisasmCheckArgs -> IO DisasmCheckArgs)]
disasmOptions =
 [ Option ['v'] ["verbose"] (NoArg setParsed) "Output the parsed disassembly guidance."
 ]
 where
   setParsed arg = return $ arg { showParsed = True }
