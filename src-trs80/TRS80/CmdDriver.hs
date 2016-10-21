{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TRS80.CmdDriver
  ( EmulatorDriver(..)
  ) where

import Control.Lens ((^.))
import System.IO
import Machine
import TRS80.System
import TRS80.Disasm

commandLineDriver :: ModelISystem -> [String] -> IO ()
commandLineDriver sys options
  | cmd == "disasm"
  = disasmCmd sys options
  | otherwise
  = do
      hPutStrLn stderr $ "TRS-80 Model I: unrecognized command: '" ++ cmd ++ "'"
  where
    cmd = head options

-- | 'EmuCommandLineDispatch' type family instance for the TRS-80 Model I System generic processor
instance EmulatorDriver ModelISystem where
  formalName sys            = sys ^. sysName
  identityNames sys         = sys ^. sysAliases
  cmdDispatch state options = commandLineDriver state options
