module TRS80.Types where

import Data.Vector.Unboxed (Vector)

import Machine
import Z80

-- | Type synonym for the TRS-80 Model I emulator
type ModelISystem = EmulatedSystem Z80state Z80addr Z80word Z80instruction

