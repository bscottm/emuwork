
module TRS80.System
  ( modelI16K
  , modelI32K
  , modelI48K
  , ModelISystem(..)
  ) where

import Data.Word
import Data.Array
import Data.Vector.Unboxed (Vector)
import Control.Lens

import Machine
import Z80

data ModelIMemory where
  ModelIMemory ::
    { _rom :: Array Word16 Word8
    , _ram :: Array Word16 Word8
    } -> ModelIMemory

makeLenses ''ModelIMemory

type ModelISystem = EmulatedSystem Z80state ModelIMemory Word16 Word8 Z80instruction

modelI16K :: ModelISystem
modelI16K = mkSystem 16

modelI32K :: ModelISystem
modelI32K = mkSystem 32

modelI48K :: ModelISystem
modelI48K = mkSystem 48

romSize :: Word16
romSize = (12 * 1024)

mkSystem :: Word16 -> ModelISystem
mkSystem sz = let maxRAM = (sz * 1024)
                  sysMem = ModelIMemory { _rom = undefined
                                        , _ram = array (romSize, maxRAM - 1) [(i, 0) | i <- [romSize..(maxRAM - 1)]]
                                        }
              in  z80generic &
                    memory %~ memInternals .~ sysMem &
                    sysName .~ ("TRS-80 Model I " ++ (show sz) ++ "K RAM") &
                    sysAliases .~ ["trs80-model-I", "trs80-model-1"]

modelIfetch :: ModelIMemory -> Word16 -> Word8
modelIfetch msys addr = (msys ^. (if addr < romSize then rom else ram)) ! addr

modelIfetchN :: ModelIMemory -> Word16 -> Int -> Vector Word8
modelIfetchN = undefined