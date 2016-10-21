{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TRS80.System
  ( modelI16K
  , modelI32K
  , modelI48K
  , trs80generic
  , ModelISystem
  ) where

import Control.Lens
import Data.Array
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed as DVU (empty)
import Data.Word

import Machine
import Z80

data ModelIMemory where
  ModelIMemory ::
    { _rom :: Array Word16 Word8
    , _ram :: Array Word16 Word8
    } -> ModelIMemory

makeLenses ''ModelIMemory

type ModelISystem = EmulatedSystem Z80state ModelIMemory Word16 Word8 Z80instruction

trs80generic :: forall memInternals. EmulatedSystem Z80state ModelIMemory Word16 Word8 Z80instruction
trs80generic = z80generic &
                 memory .~ ( z80generic ^. memory &
                               memInternals .~ ModelIMemory { _rom = undefined
                                                            , _ram = undefined
                                                            } &
                               mfetch .~ (\_addr -> 0 :: Word8) &
                               mfetchN .~ (\_addr _nBytes -> DVU.empty)) &
                    sysName .~ "TRS-80 Model I" &
                    sysAliases .~ ["trs80-model-I", "trs80-model-1"]

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
                    memory .~ (trs80generic ^. memory &
                                 memInternals .~ sysMem &
                                 mfetch .~ (modelIfetch sysMem) &
                                 mfetchN .~ (modelIfetchN sysMem))

modelIfetch :: ModelIMemory -> Word16 -> Word8
modelIfetch msys addr = (msys ^. (if addr < romSize then rom else ram)) ! addr

modelIfetchN :: ModelIMemory -> Word16 -> Int -> Vector Word8
modelIfetchN = undefined

-- | 'EmuCommandLineDispatch' type family instance for the TRS-80 Model I System generic processor
instance EmulatorDriver ModelISystem where
  formalName sys             = sys ^. sysName
  identityNames sys          = sys ^. sysAliases
  cmdDispatch _state options = putStrLn $ "TRS-80 system dispatch invoked, args = " ++ (show options)
