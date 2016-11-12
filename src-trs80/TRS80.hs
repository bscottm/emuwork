{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
The TRS-80 Model I system (and maybe, someday, a Model III and Model 4P as well.)
-}

module TRS80
  ( module TRS80.System
  , trs80generic
  , trs80CmdDispatch
  ) where

import System.IO
import System.Exit
import Control.Lens ((&), (.~))

import Machine

import TRS80.CommonOptions
import TRS80.Disasm
import TRS80.System

import Z80

-- | A very basic (and completely unusable) TRS-80 Model I system
trs80generic :: TRS80ModelISystem
trs80generic = z80generic &
                 sysName .~ "TRS-80 Model I" &
                 sysAliases .~ ["trs80-model-I", "trs80-model-1", "trs80-model-i"] &
                 cmdDispatch .~ trs80CmdDispatch

trs80CmdDispatch :: TRS80ModelISystem
                 -> [String]
                 -> IO ()
trs80CmdDispatch sys options
  | null options
  = hPutStrLn stderr "Emulator needs a command. Valid commands are:"
    >> hPutStrLn stderr "  disasm"
    >> hPutStrLn stderr ""
    >> commonOptionUsage
    >> disasmUsage
    >> exitFailure
  | cmd == "disasm"
  = disasmCmd sys (tail options)
  | otherwise
  = hPutStrLn stderr $ "TRS-80 Model I: unrecognized command: '" ++ cmd ++ "'"
  where
    cmd = head options
