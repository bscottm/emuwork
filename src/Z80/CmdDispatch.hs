-- | The Z80 emulation's command dispatch module.
--
-- The normal way of invoking these commands is:
--
-- @emuwork --processor=z80 <z80-specific command>@
--
-- Currently supported commands include:
--
-- @disassemble@, @disasm@
--   Disassemble an image, e.g., raw ROM file
module Z80.CmdDispatch
       ( z80cmdDispatch
       ) where

import Prelude hiding(catch)
import System.IO
-- redundant: import System.Environment
-- redundant: import System.FilePath
-- redundant: import System.Directory
-- redundant: import System.Exit
import System.Console.GetOpt
import Control.DeepSeq
import Control.Exception
-- redundant: import Data.Word
import Data.Maybe
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as DVU

import Reader
-- redundant: import Machine
import Z80.Processor
  
-- | Various things we can do with the Z80 processor emulator
data Z80Command = NoCommand
                | InvalidCommand [String]
                | Disassemble
                  { emuImage    :: FilePath
                  , origin      :: Z80addr
                  , startAddr   :: Maybe Z80addr
                  , nBytesToDis :: Maybe Z80disp
                  }

-- | The Z80 command line interface dispatch function
z80cmdDispatch :: Z80state
               -> [String]
               -> IO ()
z80cmdDispatch _machine options =
    case parseCmd options of
      NoCommand               -> hPutStrLn stderr "No command?"
      InvalidCommand errs     -> hPutStrLn stderr ("Invalid command" ++ (show errs))
      disAsm@Disassemble { }  -> (cmdDisassemble disAsm) `catch` (\ exc -> hPutStrLn stderr (show (exc :: ErrorCall)))

parseCmd :: [String]
         -> Z80Command
parseCmd [] = NoCommand
parseCmd (o:options)
  | o == "disassemble" || o == "disasm" = parseDisassemble options
  | otherwise = InvalidCommand ["Unrecognized command", o]

-- | Parse disassembler options.
parseDisassemble :: [String]
                 -> Z80Command
parseDisassemble opts =
  case getOpt RequireOrder disAsmOptions opts of
    (o, _, []) -> foldl' (flip id) defaultOptions o
    (_, _, errs) -> InvalidCommand errs
  where
    defaultOptions = Disassemble { emuImage = ""
                                 ,  origin = 0
                                 ,  startAddr = Nothing
                                 ,  nBytesToDis = Nothing
                                 }

disAsmOptions :: [OptDescr (Z80Command -> Z80Command)]
disAsmOptions =
  [ Option []    ["image"]      (ReqArg setRomImage "<FILE>") "Set the initial image file"
  , Option []    ["origin"]     (ReqArg setOrigin   "<ADDR>") "Set the disassembly output's origin address, default = 0"
  , Option []    ["start"]      (ReqArg setStartOffset "<ADDR>") "Disassembly start address, relative to the origin"
  , Option []    ["length"]     (ReqArg setDisLength "<DISP>") "Number of bytes to disassemble"
  ]
  where
    setRomImage imageFile flags = flags { emuImage = imageFile }
    setOrigin inp flags = flags { origin = toWord16 "setOrigin" inp }
    setStartOffset inp flags = flags { startAddr = Just (toWord16 "setStartOffset" inp) }
    setDisLength inp flags = flags { nBytesToDis = Just (read inp) }

    toWord16 whence x = case reads x of
                          (val, s) : [] -> if null s then
                                             val
                                           else
                                             error (whence ++  ": Could not parse unsigned, 16-bit number ('" ++ x ++ "')")
                          _otherwise -> error (whence ++ ": Could not parse unsigned, 16-bit number ('" ++ x ++ "')")

{- unused
-- | Show the help message:
helpMsg :: String -> String
helpMsg progname = let header = "Usage: " ++ progname ++ " [OPTIONS]"
                   in  usageInfo header disAsmOptions
-}

{- unused
-- | Show the usage message
showUsage :: IO ()
showUsage = getProgName >>= (\progname -> hPutStr stderr (helpMsg progname))
-}

{- unused
-- | Print an error message to stderr, then exit with failure status
exitError :: String
             -> IO a
exitError msg = do
    prg <- getProgName
    hPutStrLn stderr (prg ++ ": " ++ msg)
    showUsage
    exitFailure
-}

-- | Execute the Z80 disassembler!
cmdDisassemble :: Z80Command
               -> IO ()
cmdDisassemble disAsm =
  disAsm `deepseq` readRawWord8Vector (emuImage disAsm)
		   >>= \img -> let theOrigin      = origin disAsm
				   theStartAddr   = fromMaybe theOrigin (startAddr disAsm)
				   theImageLen    = DVU.length img
				   theNBytesToDis = fromMaybe (fromIntegral theImageLen) (nBytesToDis disAsm)
			       in  if not . DVU.null $ img then
				     putStrLn (intercalate "" [ "disassemble image "
							      , (show . emuImage $ disAsm)
							      , ", length "
							      , (show theImageLen)
							      , ", origin "
							      , (show theOrigin)
							      , ", start "
							      , (show theStartAddr)
							      , ", bytes to dump "
							      , (show theNBytesToDis)
							      ])
				 else
				   putStrLn "-- Error reading image"

-- | Ensure that 'Z80Command' can be fully evaluated by 'deepseq'. Notably, this is used to catch
-- integer parsing errors 
instance NFData Z80Command where
  rnf NoCommand = ()
  rnf (InvalidCommand strs) = strs `seq` ()
  rnf (Disassemble img org sAddr nBytes) = img `seq` org `seq` sAddr `seq` nBytes `seq` ()
