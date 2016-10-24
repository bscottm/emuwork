{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-- |
The venerable TRS-80 (aka "the Trash 80") system.
-}

module TRS80.System
  ( trs80generic
  , ModelISystem
  , ModelIMemory
  , trs80System
  , rom
  , ram
  ) where

import           Control.Lens
import           Data.Vector.Unboxed (Vector, (!), empty)
import qualified Data.Vector.Unboxed as DVU (replicate, generate, length)
import qualified Data.Text as T
import           Debug.Trace
import           System.IO

import           Machine
import           Machine.Utils
import           Z80

{- | The TRS-80 Model I's memory system.-}
data ModelIMemory where
  ModelIMemory ::
    { _rom :: Vector Z80word
    , _ram :: Vector Z80word
    } -> ModelIMemory

makeLenses ''ModelIMemory

-- | Type synonym for the TRS-80 Model I emulator
type ModelISystem = EmulatedSystem Z80state ModelIMemory Z80addr Z80word Z80instruction

-- | A very basic (and completely unusable) TRS-80 Model I system
trs80generic :: EmulatedSystem Z80state ModelIMemory Z80addr Z80word Z80instruction
trs80generic = z80generic &
                 memory .~ ( z80generic ^. memory &
                               memInternals .~ ModelIMemory { _rom = empty
                                                            , _ram = empty
                                                            } &
                               mfetch .~ (\_addr -> 0 :: Z80word) &
                               mfetchN .~ (\_addr _nBytes -> empty)) &
                    idecode .~ z80insnDecode &
                    sysName .~ "TRS-80 Model I" &
                    sysAliases .~ ["trs80-model-I", "trs80-model-1", "trs80-model-i"]

-- | Create the system's RAM
mkRAM :: ModelISystem
                -> Z80addr
                -> ModelISystem
mkRAM sys memSize = let maxRAM = memSize * 1024
                        sysMem = trace ("mkRAM: maxRAM = " ++ (show maxRAM)) $ DVU.replicate (fromIntegral maxRAM) 0
                        sMem = sys ^. memory
                        sysRAM = sMem & memInternals %~ (ram .~ sysMem) &
                                        mfetch  .~ (modelIfetch  (sysRAM ^. memInternals)) &
                                        mfetchN .~ (modelIfetchN (sysRAM ^. memInternals))
                    in trace "mkRAM" $ sys & memory .~ sysRAM

-- | Install the system's ROM
installROM :: ModelISystem -> Vector Z80word -> ModelISystem
installROM sys romImage =
  if DVU.length romImage == fromIntegral romSize
  then let mem = sys ^. memory ^. memInternals & rom .~ romImage
       in  trace "Installed ROM" $ sys & memory %~ memInternals .~ mem &
                                                   mfetch .~ (modelIfetch mem) &
                                                   mfetchN .~ (modelIfetchN mem)
  else trace "Invalid ROM" $ sys

{- ! Fetch a byte from memory. The TRS-80 has a very simple memory layout:

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
modelIfetch :: ModelIMemory -> Z80addr -> Z80word
modelIfetch msys addr
  | trace ("modelIfetch " ++ (T.unpack $ as0xHex addr)) False = undefined
  | addr < romSize - 1
  = theROM ! fromIntegral addr
  | addr >= mmapIOStart && addr < mmapIOEnd
  {- FIXME -}
  = 0
  | addr >= ramStart && addr <= ramEnd
  = theRAM ! fromIntegral (addr - ramStart)
  | otherwise
  = error ("TRS80.mfetch: Illegal address or invalid memory system: " ++ (show addr))
  where
    theROM = msys ^. rom
    theRAM = msys ^. ram

modelIfetchN :: ModelIMemory -> Z80addr -> Int -> Vector Z80word
modelIfetchN msys start nbytes = let fetchByte idx = modelIfetch msys (start + fromIntegral idx)
                                 in  DVU.generate nbytes fetchByte

romSize, mmapIOStart, mmapIOEnd, ramStart, ramEnd :: Z80addr
romSize     = (12 * 1024)
mmapIOStart = 0x3000
mmapIOEnd   = mmapIOStart + 0x1000
ramStart    = (16 * 1024)
ramEnd      = (64 * 1024) - 1

trs80System :: FilePath
            -> (FilePath -> IO (Vector Z80word))
            -> Z80addr
            -> ModelISystem
            -> IO ModelISystem
trs80System romPath reader memSize trs80 =
    reader romPath
    >>= (\romImage -> return $ installROM (mkRAM trs80 memSize) romImage)
