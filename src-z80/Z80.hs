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
import           Z80.InsnExecute    as Z80exports
import           Z80.InstructionSet as Z80exports
import           Z80.Processor      as Z80exports
import           Z80.System         as Z80exports

-- import           Z80.InstructionSet (Z80instruction)
-- import           Z80.Processor      (Z80state, z80initialState)
-- import           Z80.System         (Z80memory)
import           Z80.InstructionSet ()
import           Z80.Processor      ()
import           Z80.System         ()

-- | The Z80 processor instantiation.
z80processor :: Z80emulation
z80processor = EmulatedProcessor
               { _procPrettyName = "Zilog Z80"
               , _cpu            = z80initialState
               , _ops            = ProcessorOps
                                   { _idecode = z80InsDecode
                                   , _iexecute = z80instructionExecute
                                   }
               }

-- | Type discriminant for the *z80generic* system. Other Z80-based systems
-- should use a different discriminant.
data Z80BaseSystem

-- | Generic Z80 system, from which other systems are derived. This is not an actual or useful
-- system because it has no predefined memory regions. The TRS-80, for example, adds a 12K ROM
-- region and RAM.
z80generic :: Z80system Z80BaseSystem
z80generic = mkEmulatedSystem z80processor name aliases
  where
    name    = "Generic Z80 system"
    aliases = ["z80generic", "Z80-generic"]
