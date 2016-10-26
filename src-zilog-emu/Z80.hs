{-# LANGUAGE RankNTypes #-}

-- | Re-export module for the Zilog Z80 processor.
module Z80
       ( module Z80.Processor
       , module Z80.InstructionSet
       , module Z80.InsnDecode
       , module Z80.Disassembler
       , module Z80.DisasmOutput
       -- , module Z80.ParseAnalytic
       , z80processor
       , z80generic
       , Z80genericMem(..)
       ) where

import           Control.Lens (makeLenses, (^.))
import           Data.Vector.Unboxed (Vector, (!), slice)
import qualified Data.Vector.Unboxed as DVU (length, replicate)
import           Data.Word

import           Machine

import           Z80.Processor
import           Z80.InstructionSet
import           Z80.Disassembler
import           Z80.DisasmOutput
-- import Z80.ParseAnalytic

import           Z80.InsnDecode

-- | Z80 processor.
z80processor :: EmulatedProcessor Z80state Word16 Z80instruction
z80processor = EmulatedProcessor
               { _procPrettyName = "Zilog Z80"
               , _internals      = z80initialState
               }

-- | Generic Z80 system, used for disassembling ROMs and such. This is not an actual or useful
-- system.
z80generic :: EmulatedSystem Z80state Word16 Word8 Z80instruction
z80generic = EmulatedSystem
             { _processor = z80processor
             , _memory    = MemorySystem (Z80genericMem { _ram = DVU.replicate (fromIntegral (maxBound :: Z80addr)) 0 })
             , _idecode    = z80insnDecode
             , _sysName    = "Generic Z80 system"
             , _sysAliases = ["z80generic", "Z80-generic"]
             }

-- | A generic Z80 system's memory. It's all RAM. No ROM.
data Z80genericMem where
  Z80genericMem ::
    {
      _ram :: Vector Z80word
    } -> Z80genericMem

makeLenses ''Z80genericMem

-- | Memory operations on the generic Z80's memory system.
instance MemoryOps Z80genericMem Z80addr Z80word where
  mFetch msys addr = (msys ^. ram) ! (fromIntegral addr)
  mFetchN msys addr nbytes = let theRAM = msys ^. ram
                                 nbytes' = max (fromIntegral addr + nbytes) (DVU.length theRAM)
                             in  slice (fromIntegral addr) nbytes' theRAM
