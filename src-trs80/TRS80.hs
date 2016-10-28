{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
The TRS-80 Model I system (and maybe, someday, a Model III and Model 4P as well.)
-}

module TRS80
  ( module TRS80.Types
  , module TRS80.System
  ) where

import System.IO
import Control.Lens ((^.))

import Machine

import TRS80.Types
import TRS80.System
import TRS80.Disasm

-- | 'EmulatorDriver' type family instance for the TRS-80 Model I base system
instance EmulatorDriver ModelISystem where
  formalName sys            = sys ^. sysName
  identityNames sys         = sys ^. sysAliases
  cmdDispatch sys options
    | cmd == "disasm"
    = disasmCmd sys (tail options)
    | otherwise
    = do
        hPutStrLn stderr $ "TRS-80 Model I: unrecognized command: '" ++ cmd ++ "'"
    where
      cmd = head options
