{- | Re-export module for the Zilog Z80 processor.
-}
module Z80
       ( module Z80exports
       , z80processor
       , z80generic
       , Z80BaseSystem
       ) where

import           Machine

import           Z80.DisasmOutput   as Z80exports
import           Z80.Disassembler   as Z80exports
import           Z80.InsnDecode     as Z80exports
import           Z80.InstructionSet as Z80exports
import           Z80.Processor      as Z80exports
-- import Z80.ParseAnalytic

import           Z80.InstructionSet (Z80instruction)
import           Z80.Processor      (Z80memory, Z80state, z80initialState)

-- | The Z80 processor instantiation.
z80processor :: Z80emulation
z80processor = EmulatedProcessor
               { _procPrettyName = "Zilog Z80"
               , _internals      = z80initialState
               }

-- | Type discriminant for the *z80generic* system. Other Z80-based systems
-- should use a different discriminant.
data Z80BaseSystem

-- | Generic Z80 system, from which other systems are derived. This is not an actual or useful
-- system because it has no predefined memory regions. The TRS-80, for example, adds a 12K ROM
-- region and RAM.
z80generic :: Z80system Z80BaseSystem
z80generic = EmulatedSystem
             { _processor   = z80processor
             , _memory      = initialMemorySystem :: Z80memory
             , _sysName     = "Generic Z80 system"
             , _sysAliases  = ["z80generic", "Z80-generic"]
             }
