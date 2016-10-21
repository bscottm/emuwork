
module TRS80.System
  ( modelI16K
  , modelI32K
  , modelI48K
  ) where

import Data.Word
import Data.Array
import Control.Lens

import Machine
import Z80

data ModelIMemory where
  ModelIMemory ::
    { _rom :: Array Word16 Word8
    , _ram :: Array Word16 Word8
    } -> ModelIMemory

mkSystem :: Word16 -> EmulatedSystem Z80state ModelIMemory Word16 Word16 Z80instruction
mkSystem sz = let maxROM = (12 * 1024)
                  maxRAM = (sz * 1024)
                  sysMem = ModelIMemory { _rom = undefined
                                        , _ram = array (maxROM, maxRAM - 1) [(i, 0) | i <- [maxROM..(maxRAM - 1)]]
                                        }
              in  z80generic & memory %~ memInternals .~ sysMem &
                    sysName .~ "TRS-80 Model I" &
                    sysAliases .~ ["trs80-model-I", "trs80-model-1"]
                  
modelI16K :: EmulatedSystem Z80state ModelIMemory Word16 Word16 Z80instruction
modelI16K = mkSystem 16

modelI32K :: EmulatedSystem Z80state ModelIMemory Word16 Word16 Z80instruction
modelI32K = mkSystem 32

modelI48K :: EmulatedSystem Z80state ModelIMemory Word16 Word16 Z80instruction
modelI48K = mkSystem 48
