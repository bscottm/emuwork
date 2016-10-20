{- |
  The TRS-80 memory system.
-}

module TRS80.Memory
  ( ModelIMem(..)
  , modelI16K
  , modelI32K
  , modelI48K
  ) where

import Data.Array
import Data.Word

-- | Model I's memory is very straightforward: 12K of ROM, everything else is RAM.
data ModelIMem where
  ModelIMem ::
    {
      -- | ROM area
      _rom :: Array Word16 Word8
      -- | RAM area
    , _ram :: Array Word16 Word8
    } -> ModelIMem

-- | Create the TRS-80's memory internal structure. ROM is held undefined until its contents are loaded later.
mkMem :: Word16 -> ModelIMem
mkMem ramSize = let lim12k = 12*1024
                    ramlim = (ramSize * 1024) - lim12k
                in  ModelIMem { _rom = undefined
                              , _ram = array (0, ramlim - 1) [(i, 0) | i <- [0..ramlim - 1]]
                              }

modelI16K :: ModelIMem
modelI16K = mkMem 16

modelI32K :: ModelIMem
modelI32K = mkMem 32

modelI48K :: ModelIMem
modelI48K = mkMem 48