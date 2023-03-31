{- |
The TRS-80 Model I system (and maybe, someday, a Model III and Model 4P as well.)
-}

module Main where

import           Control.Monad         (foldM, unless)
import           Data.List             (intercalate)

import System.IO
import System.Environment (getArgs, getProgName)
import System.Exit ( exitFailure, exitSuccess )
import System.Console.GetOpt

main :: IO ()
main =
  (getArgs >>= combineArgs . getOpt RequireOrder trs80Options)
  >> exitSuccess
  where
    combineArgs :: ([TRS80Options -> IO TRS80Options], [String], [String]) -> IO ()
    combineArgs (optsActions, rest, errs) =
          unless (null errs) (hPutStrLn stderr (intercalate "\n" errs) >> showUsage >> exitFailure)
          >> unless (null rest) showUsage
          >> foldM (flip ($)) mkTRS80Options optsActions
          >> return ()


    -- | Show the help message:
    helpMsg :: String -> String
    helpMsg progname = let header = "Usage: " ++ progname ++ " [OPTIONS]"
                       in  usageInfo header trs80Options

    -- | Show the usage message
    showUsage :: IO ()
    showUsage = getProgName >>= hPutStr stderr . helpMsg

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data TRS80Options where
  -- | Collected options to configure a TRS-80 Model I system. This is normally what gets
  -- returned.
  TRS80Options :: TRS80Options

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

mkTRS80Options :: TRS80Options
mkTRS80Options = TRS80Options

trs80Options :: [OptDescr (TRS80Options -> IO TRS80Options)]
trs80Options =
  [
  ]