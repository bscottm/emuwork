{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-- |
The venerable TRS-80 (aka "the Trash 80") system.
-}

module TRS80.System
  ( trs80generic
  , ModelISystem
  , trs80System
  ) where

import Control.Lens
import Data.Array
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed as DVU (empty)
import Data.Word

import Machine
import Z80

{- | The TRS-80 Model I's memory system. It's a very simple layout:

Address (hex) 	Description
0000-2FFF 	Level II ROM
3000-37DF 	Unused
37E0-37FF 	Memory Mapped I/O
3800-38FF 	Keyboard map
3900-3BFF 	(Keyboard 'shadow'ed here)
3C00-3FFF 	1kb Video RAM
4000-41FF 	RAM used by the ROM routines
4200-7FFF 	Usable RAM in a 16K machine
8000-BFFF 	Additional RAM in a 32K machine
C000-FFFF 	Still more in a 48K machine
-}
data ModelIMemory where
  ModelIMemory ::
    { _rom :: Vector Word8
    , _ram :: Array Word16 Word8
    } -> ModelIMemory

makeLenses ''ModelIMemory

-- | Type synonym for the TRS-80 Model I emulator
type ModelISystem = EmulatedSystem Z80state ModelIMemory Word16 Word8 Z80instruction

-- | A very basic (and completely unusable) TRS-80 Model I system
trs80generic :: EmulatedSystem Z80state ModelIMemory Word16 Word8 Z80instruction
trs80generic = z80generic &
                 memory .~ ( z80generic ^. memory &
                               memInternals .~ ModelIMemory { _rom = undefined
                                                            , _ram = undefined
                                                            } &
                               mfetch .~ (\_addr -> 0 :: Word8) &
                               mfetchN .~ (\_addr _nBytes -> DVU.empty)) &
                    sysName .~ "TRS-80 Model I" &
                    sysAliases .~ ["trs80-model-I", "trs80-model-1", "trs80-model-i"]

romSize :: Word16
romSize = (12 * 1024)

ramStart :: Word16
ramStart = (16 * 1024)

-- | Create the system's RAM
mkRAM :: ModelISystem
                -> Word16
                -> ModelISystem
mkRAM sys memSize = let maxRAM = memSize * 1024
                        sysMem = array (ramStart, ramStart + maxRAM - 1)
                                       [(i + ramStart, 0) | i <- [0..(maxRAM - 1)]]
                        sysRAM = (sys ^. memory) & memInternals %~ (ram .~ sysMem)
                    in sys & memory .~ (sysRAM & mfetch  .~ (modelIfetch  (sysRAM ^. memInternals))
                                               & mfetchN .~ (modelIfetchN (sysRAM ^. memInternals)))

-- | Install the system's ROM
installROM :: ModelISystem -> Vector Word8 -> ModelISystem
installROM sys romImage = sys & memory %~ (memInternals %~ (rom .~ romImage))

modelIfetch :: ModelIMemory -> Word16 -> Word8
modelIfetch msys addr = (msys ^. (if addr < romSize then rom else ram)) ! addr

modelIfetchN :: ModelIMemory -> Word16 -> Int -> Vector Word8
modelIfetchN = undefined

trs80System :: (Monad m) =>
               FilePath
            -> (FilePath -> m (Vector Word8))
            -> Word16
            -> ModelISystem
            -> m ModelISystem
trs80System romPath reader memSize trs80 =
  do
    romImage <- reader romPath
    return $ installROM (mkRAM trs80 memSize) romImage