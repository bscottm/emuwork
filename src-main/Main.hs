-- | The main module, where all of the fun happens.
module Main (main) where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.List
import Control.Monad

import qualified Machine
import qualified Z80

main::IO ()
main =
  do
    (flags, others) <- parseOptions
    case Machine.emulator flags of
      Nothing          -> hPutStrLn stderr "Emulator not specified on command line with '--processor.' flag."
                          >> dumpEmulators
      Just theEmulator -> doDispatch theEmulator others

doDispatch :: Machine.EmulatedProcessor
           -> [String]
           -> IO ()
doDispatch (Machine.EmulatedProcessor _machine _names dispatch theInternals) emuOpts = dispatch theInternals emuOpts

-- | Lookup an 'Machine.EmulatedProcessor' from an internal list of known
-- emulators.
lookupEmulator :: String
               -> Maybe Machine.EmulatedProcessor
lookupEmulator emuname = lookupEmulator' emuname knownProcessorEmulators
  where
    lookupEmulator' _name [] = Nothing
    lookupEmulator' name (e:emus) = if (name `elem` (Machine.names e))
                                    then Just e
                                    else lookupEmulator' name emus

-- | List of known emulators. This is different from a list of known emulated systems.
knownProcessorEmulators :: [Machine.EmulatedProcessor]
knownProcessorEmulators =
  [ Machine.nullProcessor
  , Z80.z80processor
  ]

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
    setEmulator emuname flags   = 
      case theEmulator of
        Nothing         -> do
          hPutStrLn stderr ("Unrecognized emulator name: " ++ emuname)
          dumpEmulators
          exitFailure
        Just _something -> return flags { Machine.emulator = theEmulator }
      where
        theEmulator = lookupEmulator emuname

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

-- | Pretty print the known emulator list:
dumpEmulators :: IO ()
dumpEmulators = do
  hPutStrLn stderr "Known emulators are:" >> mapM_ prettyEmu knownProcessorEmulators
  where
    prefix = "-- "
    indent = "   "
    indentLen = length indent
    initialString emu = prefix ++ (Machine.machineName emu) ++ " ("

    prettyEmu emu = do
      hPutStr stderr (initialString emu)
      prettyEmuNames (Machine.names emu) (length (initialString emu))

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
