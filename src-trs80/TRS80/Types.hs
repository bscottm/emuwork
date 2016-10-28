module TRS80.Types where

import Data.Vector.Unboxed (Vector)

import Machine
import Z80

-- | The TRS-80 Model I's memory system.
data ModelIMemory where
  ModelIMemory ::
    { rom :: Vector Z80word
    , ram :: Vector Z80word
    } -> ModelIMemory

-- | Type synonym for the TRS-80 Model I emulator
type ModelISystem = EmulatedSystem Z80state Z80addr Z80word Z80instruction

