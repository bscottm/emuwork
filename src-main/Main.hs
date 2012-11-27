{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The main module, where all of the fun happens.
module Main (main) where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.List
import Control.Monad
import Control.Lens

import qualified Machine
import qualified Z80

-- Emit Template Haskell hair for the lenses
makeLenses ''Machine.EmulatedProcessor

-- | Where all of the emulator stuff starts...
main::IO ()
main =
  do
    (flags, others) <- parseOptions
    case Machine.emulator flags of
      ""      -> hPutStrLn stderr "Emulator not specified on command line with '--processor.' flag."
                 >> dumpEmulators
      emuName  -> doDispatch emuName others

-- | Dispatch the command to the appropriate emulator.
--
-- (Wish this could be list-driven, but the type system doesn't allow the kind of type-based polymorphism
-- required as the result of type variable rigidity.)
doDispatch :: String                            -- ^ The requested emulator's name
           -> [String]                          -- ^ Command line (emulator-specific)
           -> IO ()
doDispatch emuName emuOpts
  | nullproc <- Machine.nullProcessor, Machine.procIdentify nullproc emuName =
    Machine.cmdDispatch nullproc emuOpts
  | z80proc  <- Z80.z80processor, Machine.procIdentify z80proc emuName =
    Machine.cmdDispatch z80proc emuOpts
  | otherwise =
    hPutStrLn stderr ("Unsupported or unknown processor emulator: '" ++ (show emuName) ++ "'")
    >> dumpEmulators

-- | Parse command line flags and options, returning a 'Machine.CommonEmulatorOptions' record (options) and
-- remaining command line parameters wrapped in the IO monad
parseOptions :: IO (Machine.CommonEmulatorOptions, [String])
parseOptions =
    let processArgs         = getArgs >>= return . getOpt RequireOrder options
        -- The key observation here is that within each option in the OptDescr list,
        -- there is a function that sets an individual member of the 'Machine.CommonEmulatorOptions'
        -- record.
        --
        -- When getOpt processes the command line options, it will create a list
        -- of the options that are actually present on the command line. The
        -- getOpt-generated list is then combined via foldl', which only modifies
        -- the parts of the record that occurred on the command line. The result
        -- is a single (IO Machine.CommonEmulatorOptions) object that has the
        -- default options set, modified by the options present in getArgs
        foldArgsAndDefaults = foldl' (>>=) (return Machine.defaultCommonEmulatorOptions)
        -- Yell at the user if there were option/flag processing errors
        checkErrors errors  = unless (null errors) $ do
                                mapM_ (hPutStrLn stderr) errors
                                showUsage
                                exitFailure
    in do
      (optsActions, rest, errs) <- processArgs
      -- Check for argument errors
      checkErrors errs
      -- Fold the flag functions and combine the results into a single
      -- return value
      opts <- foldArgsAndDefaults optsActions
      -- Return the IO (Machine.CommonEmulatorOptions, [String]) result
      return (opts, rest)

-- | Common command line options
options :: [OptDescr (Machine.CommonEmulatorOptions -> IO Machine.CommonEmulatorOptions)]
options =
  [ Option []    ["processor"]  (ReqArg setEmulator "<NAME>") "Set the processor emulator"
  , Option ['?'] ["help"]       (NoArg  doUsage)              "Get help"
  ]
  where
    setEmulator emuname flags   = return $ flags { Machine.emulator = emuname }

    doUsage           _flags    = do 
      showUsage
      exitWith ExitSuccess

-- | Show the help message:
helpMsg :: String -> String
helpMsg progname = let header = "Usage: " ++ progname ++ " [OPTIONS]"
                   in  usageInfo header options

-- | Show the usage message
showUsage :: IO ()
showUsage = getProgName >>= (\progname -> hPutStr stderr (helpMsg progname))

{- unused:
-- | Print an error message to stderr, then exit with failure status
exitError :: String
             -> IO a
exitError msg = do
    prg <- getProgName
    hPutStrLn stderr (prg ++ ": " ++ msg)
    showUsage
    exitFailure
-}

-- | Dump the known emulators
dumpEmulators :: IO ()
dumpEmulators = do
    hPutStrLn stderr "Available emulators are:"
      >> prettyEmu Machine.nullProcessor
      >> prettyEmu Z80.z80processor
  where
    prefix = "-- "
    indent = "   "
    indentLen = length indent
    initialString emu = prefix ++ (emu ^. procPrettyName) ++ " ("

    prettyEmu emu = do
      hPutStr stderr (initialString emu)
      prettyEmuNames (emu ^. procAliases) (length (initialString emu))

    prettyEmuNames []         _accLen = hPutStrLn stderr ""
    prettyEmuNames (eName:[]) _accLen = hPutStrLn stderr ("\"" ++ eName ++ "\")")
    prettyEmuNames emuNames@(eName:eNames) accLen
      | accLen == 0 =
          do
            hPutStr stderr (indent ++ "\"" ++ eName ++ "\"")
            prettyEmuNames eNames (indentLen + length eName + 2)
      | length eName + accLen > 80 =
          do
            hPutStrLn stderr ""
            prettyEmuNames emuNames 0
      | otherwise =
          do
            hPutStr stderr ("\"" ++ eName ++ "\", ")
            prettyEmuNames eNames (accLen + length eName + 4)
