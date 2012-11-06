{-# LANGUAGE FlexibleInstances #-}
module Z80.CmdDispatch
       ( z80cmdDispatch
       ) where

import Prelude hiding(catch)
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import System.Exit
import System.Console.GetOpt
import Control.Exception
import Data.Word
import Data.Maybe
import Data.List (foldl')
import qualified Data.Vector.Unboxed as DVU

import Reader
import Machine
import Z80.Processor

instance CmdEnvDispatch z80processor where
  cmdDispatch = z80cmdDispatch
  
-- | Various things we can do with the Z80 processor emulator
data Z80Command = NoCommand
                | InvalidCommand [String]
                | Disassemble
                  { emuImage  :: FilePath
                  , origin :: Z80addr
                  , startAddr :: Maybe Z80addr
                  , nBytesToDis :: Maybe Z80disp
                  }

-- | The Z80 command line interface dispatch function
z80cmdDispatch :: z80processor
               -> CmdEnvironment
               -> [String]
               -> IO ()
z80cmdDispatch _machine cmdEnv options =
  do
    putStrLn "Z80 processor dispatch"
    case parseCmd options of
      NoCommand               -> hPutStrLn stderr "No command?"
      InvalidCommand errs     -> hPutStrLn stderr ("Invalid command" ++ (show errs))
      disAsm@Disassemble { }  -> (cmdDisassemble disAsm) `catch` (\ e -> hPutStrLn stderr (show (e :: ErrorCall)))

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
  , Option []    ["origin"]     (ReqArg setOrigin   "<ADDR>") "Set the disassembly output's origin address"
  , Option []    ["start"]      (ReqArg setStartOffset "<ADDR>") "Set the disassembly start address"
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

-- | Show the help message:
helpMsg :: String -> String
helpMsg progname = let header = "Usage: " ++ progname ++ " [OPTIONS]"
                   in  usageInfo header disAsmOptions

-- | Show the usage message
showUsage :: IO ()
showUsage = getProgName >>= (\progname -> hPutStr stderr (helpMsg progname))

-- | Print an error message to stderr, then exit with failure status
exitError :: String
             -> IO a
exitError msg = do
    prg <- getProgName
    hPutStrLn stderr (prg ++ ": " ++ msg)
    showUsage
    exitFailure

-- | Execute the Z80 disassembler!
cmdDisassemble :: Z80Command
               -> IO ()
cmdDisassemble disAsm =
  -- Note that the interior of disAsm is
  let evalDisAsm = origin disAsm `seq` startAddr disAsm `seq` nBytesToDis disAsm
  in  evalDisAsm `seq` readRawWord8Vector (emuImage disAsm)
                         >>= \img -> if not . DVU.null $ img then
                                       putStrLn ("Successfully read image, length " ++ (show (DVU.length img)))
                                         >> putStrLn ("Disassembly origin = " ++ (show (origin disAsm)))
                                     else
                                       putStrLn "-- Error reading image"