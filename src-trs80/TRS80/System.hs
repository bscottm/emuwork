{-# LANGUAGE FlexibleInstances    #-}

{- |
The venerable TRS-80 (aka "the Trash 80") system.
-}

module TRS80.System
  ( trs80System
  , TRS80ModelISystem
  ) where

import           Lens.Micro          ((&), (%~))
import           Data.Vector.Unboxed (Vector)

import           Machine
import           Z80

-- | The *Z80system* system discriminant
data TRS80ModelITag

-- | The TRS-80 Model I system type. It's a specific type of Z80-based system.
type TRS80ModelISystem = Z80system TRS80ModelITag

-- | Create the system's RAM
installMem :: TRS80ModelISystem
           -> Int
           -> Vector Z80word
           -> TRS80ModelISystem
installMem sys memSize newROM = sys & memory %~ (mkROMRegion 0 newROM . mkRAMRegion ramStart (memSize * 1024))

{- The TRS-80 has a very simple memory layout:

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

{-
mmapIOStart, mmapIOEnd, ramStart :: Z80addr
-- | Start of memory mapped I/O adress space.
mmapIOStart = 0x3000
-- | End of memory mapped I/O address space.
mmapIOEnd   = mmapIOStart + 0x1000
-}

-- | Start of usable RAM
ramStart :: Z80addr
ramStart    = 16 * 1024

-- | TRS-80 Model I constructor: install a ROM image and configure the system's RAM.
trs80System :: FilePath
            -- ^ File path to the ROM image
            -> (FilePath -> IO (Vector Z80word))
            -- ^ ROM image reader (RAW vs. Intel Hex vs. Hex strings)
            -> Int
            -- ^ Memory size: 16K, 32K or 48K
            -> TRS80ModelISystem
            -- ^ Initial Model I system (should be 'trs80generic')
            -> IO TRS80ModelISystem
            -- ^ Fully constructed TRS-80 Model I
trs80System romPath reader memSize trs80 = installMem trs80 memSize <$> reader romPath
