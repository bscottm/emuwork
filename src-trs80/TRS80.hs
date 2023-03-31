{- |
The TRS-80 Model I system (and maybe, someday, a Model III and Model 4P as well.)
-}

module TRS80
  ( module TRS80.System
  , module TRS80.Disasm
  , module TRS80.Disasm.Guidance
  , trs80generic
  ) where

import TRS80.Disasm
import TRS80.Disasm.Guidance
import TRS80.System

import Lens.Micro.Platform ((.~), (&))

import Machine.System
import Z80

-- | A very basic (and completely unusable) TRS-80 Model I system. Primarily useful for
-- ROM and image file disassembly.
trs80generic :: TRS80ModelISystem
trs80generic = z80generic & sysName .~ "TRS-80 Model I"
                          & sysAliases .~ ["trs80-model-I", "trs80-model-1", "trs80-model-i"]