{-# LANGUAGE RankNTypes #-}

-- | Re-export module for the Zilog Z80 processor.
module Z80
       ( module Z80exports
       , z80processor
       , z80generic
       ) where

import           Data.Word

import           Machine

import           Z80.Processor as Z80exports
import           Z80.InstructionSet as Z80exports
import           Z80.Disassembler as Z80exports
import           Z80.DisasmOutput as Z80exports
import           Z80.InsnDecode as Z80exports
-- import Z80.ParseAnalytic

import Z80.Processor (Z80state, Z80memory, z80initialState)
import Z80.InstructionSet (Z80instruction)

-- | Z80 processor.
z80processor :: EmulatedProcessor Z80state Word16 Z80instruction
z80processor = EmulatedProcessor
               { _procPrettyName = "Zilog Z80"
               , _internals      = z80initialState
               }

-- | Type discriminant for the *z80generic* system. Other Z80-based systems
-- should use a different discriminant.
data Z80BaseSystem

-- | Generic Z80 system, used for disassembling ROMs and such. This is not an actual or useful
-- system.
z80generic :: Z80system Z80BaseSystem
z80generic = EmulatedSystem
             { _processor   = z80processor
             , _memory      = initialMemorySystem :: Z80memory
             , _sysName     = "Generic Z80 system"
             , _sysAliases  = ["z80generic", "Z80-generic"]
             , _cmdDispatch = undefined
             }
