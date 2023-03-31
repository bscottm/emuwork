{-# LANGUAGE ExistentialQuantification #-}
-- | The main module, where all of the fun happens.
module Main (main) where

import           Control.Monad
import           Data.Word
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import qualified Machine
import qualified TRS80

-- | Common command line option data record
data CommonEmulatorOptions where
  EmulatorOptions ::
    { emulator :: String                  -- ^ The processor emulator's name
    } -> CommonEmulatorOptions

-- | The default emulator options
defaultCommonEmulatorOptions :: CommonEmulatorOptions
defaultCommonEmulatorOptions = EmulatorOptions
                         { emulator = ""
                         }

-- | Where all of the emulator stuff starts...
main::IO ()
main =
  do
    (flags, others) <- parseOptions
    case emulator flags of
      ""      -> hPutStrLn stderr "Emulator not specified on command line with '--system.' flag."
                 >> dumpEmulators
      emuName  -> doDispatch emuName others

class CmdLineDispatch emu where
  identify :: String
           -> emu
           -> Bool
  formalName :: emu
             -> String
  knownAs :: emu
          -> [String]
  cmdLineDispatch :: emu
                  -> [String]
                  -> IO ()

instance CmdLineDispatch AnyEmulator where
  identify emuName (AnyEmulator emu) = identify emuName emu
  formalName (AnyEmulator emu) = formalName emu
  knownAs (AnyEmulator emu) = knownAs emu
  cmdLineDispatch (AnyEmulator emu) = cmdLineDispatch emu

instance CmdLineDispatch (Machine.NullSystem Word32 Word32) where
  identify emuName emu = emuName `elem` emu ^. Machine.sysAliases
  formalName emu       = emu ^. Machine.sysName
  knownAs emu          = emu ^. Machine.sysAliases
  cmdLineDispatch      = Machine.nullProcCmdDispatch

instance CmdLineDispatch TRS80.TRS80ModelISystem where
  identify emuName emu = emuName `elem` (emu ^. Machine.sysAliases)
  formalName emu       = emu ^. Machine.sysName
  knownAs emu          = emu ^. Machine.sysAliases
  cmdLineDispatch      = TRS80.trs80CmdDispatch

data AnyEmulator where
  AnyEmulator :: CmdLineDispatch emu => emu -> AnyEmulator

knownEmulators :: [ AnyEmulator ]
knownEmulators = [ AnyEmulator (Machine.nullSystem :: Machine.NullSystem Word32 Word32)
                 , AnyEmulator TRS80.trs80generic
                 ]

-- | Dispatch the command to the appropriate emulator.
--
-- (Wish this could be list-driven, but the type system doesn't allow the kind of type-based polymorphism
-- required as the result of type variable rigidity.)
doDispatch :: String                            -- ^ The requested emulator's name
           -> [String]                          -- ^ Command line (emulator-specific)
           -> IO ()
doDispatch emuName emuOpts =
  let candEmulators = filter (identify emuName) knownEmulators
  in  if length candEmulators == 1
      then cmdLineDispatch (head candEmulators) emuOpts
      else hPutStrLn stderr
                     (if null candEmulators
                      then "Unsupported or unknown emulator: '" ++ emuName ++ "'"
                      else "Ambiguous emulator: '" ++ emuName ++ "'")
             >> dumpEmulators

-- | Parse command line flags and options, returning a 'CommonEmulatorOptions' record (options) and
-- remaining command line parameters wrapped in the IO monad
parseOptions :: IO (CommonEmulatorOptions, [String])
parseOptions =
    let processArgs         = getOpt RequireOrder options <$> getArgs
        -- The key observation here is that within each option in the OptDescr list,
        -- there is a function that sets an individual member of the 'Machine.CommonEmulatorOptions'
        -- record.
        --
        -- When getOpt processes the command line options, it will create a list
        -- of the options that are actually present on the command line. The
        -- getOpt-generated list is then combined via foldl, which only modifies
        -- the parts of the record that occurred on the command line. The result
        -- is a single (IO Machine.CommonEmulatorOptions) object that has the
        -- default options set, modified by the options present in getArgs
        foldArgsAndDefaults = foldl (>>=) (return defaultCommonEmulatorOptions)
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
options :: [OptDescr (CommonEmulatorOptions -> IO CommonEmulatorOptions)]
options =
  [ Option []    ["system"]     (ReqArg setEmulator "<NAME>") "Set the system emulator"
  , Option ['?'] ["help"]       (NoArg  doUsage)              "Get help"
  ]
  where
    setEmulator emuname flags   = return $ flags { emulator = emuname }

    doUsage           _flags    = do
      showUsage
      exitSuccess

-- | Show the help message:
helpMsg :: String -> String
helpMsg progname = let header = "Usage: " ++ progname ++ " [OPTIONS]"
                   in  usageInfo header options

-- | Show the usage message
showUsage :: IO ()
showUsage = getProgName >>= hPutStr stderr . helpMsg

-- | Dump the known emulators
dumpEmulators :: IO ()
dumpEmulators =
    hPutStrLn stderr ""
      >> hPutStrLn stderr "Available emulators are:"
      >> mapM_ prettyEmu knownEmulators
  where
    prefix = ".. "

    initialString :: AnyEmulator -> String
    initialString emu = prefix ++ formalName emu

    prettyEmu :: AnyEmulator -> IO ()
    prettyEmu emu = do
      progName <- getProgName
      hPutStrLn stderr (initialString emu)
      prettyEmuNames progName (knownAs emu)

    prettyEmuNames _    []             = hPutStrLn stderr ""
    prettyEmuNames prog (eName:eNames) = hPutStrLn stderr ("  " ++ prog ++ " --system=" ++ eName ++ " [options]")
                                         >> prettyEmuNames prog eNames
