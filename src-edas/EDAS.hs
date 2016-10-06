module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Control.Monad

import qualified Data.Foldable as Foldable
import qualified Data.Text.IO as TIO

import MisosysEDAS

data EDASOptions =
  EDASOptions
  { outputFile :: FilePath
  }

mkEDASOptions :: EDASOptions
mkEDASOptions = EDASOptions
                { outputFile = ""
                }

utilOptions :: [OptDescr (EDASOptions -> IO EDASOptions)]
utilOptions =
 [ Option [] ["output"] (ReqArg setOutputFile "<file>") "Output file name"
 ]
 where
  setOutputFile path flags = return $ flags { outputFile = path }

showUsage :: IO ()
showUsage = do
  prog <- getProgName
  hPutStrLn stderr ("Usage: " ++ prog ++ " --output=<file>")
  exitFailure

main :: IO ()
main =
    getArgs
    >>= return . getOpt RequireOrder utilOptions
    >>= (\(optsActions, rest, errs) ->
          (unless (null errs) $ do
             mapM_ (hPutStrLn stderr) errs
             >> showUsage
             >> exitFailure)
          >> Foldable.foldl' (>>=) (return mkEDASOptions) optsActions
          >>= (\_options ->
                if length rest == 1 then
                  do
                    edasOut <- edasParseFile (head rest)
                    case edasOut of
                      Left errmsg  -> TIO.hPutStrLn stderr errmsg
                      Right result -> putStrLn (show result)
                else
                  showUsage))
