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
       ) where

import qualified Data.Vector.Unboxed as DVU
import Data.Word

import Machine

import Z80.Processor
import Z80.InstructionSet
import Z80.Disassembler
import Z80.DisasmOutput
-- import Z80.ParseAnalytic

import Z80.InsnDecode

-- | Z80 processor.
z80processor :: EmulatedProcessor Z80state Word16 Z80instruction
z80processor = EmulatedProcessor
               { _procPrettyName = "Zilog Z80"
               , _internals      = z80initialState
               }

-- | Generic Z80 system, used for disassembling ROMs and such. This is not an actual or useful
-- system.
z80generic :: forall memInternals. EmulatedSystem Z80state memInternals Word16 Word8 Z80instruction
z80generic = EmulatedSystem
             { _processor = z80processor
             , _memory    = MemorySystem
                            { _memInternals = undefined
                            , _mfetch  = (\_addr -> 0 :: Word8)
                            , _mfetchN = (\_addr _nBytes -> DVU.empty)
                            , _maxmem  = 65535 :: Word16
                            }
             , _idecode    = z80insnDecode
             , _sysName    = "Generic Z80 system"
             , _sysAliases = ["z80generic", "Z80-generic"]
             }
