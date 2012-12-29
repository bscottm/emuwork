-- | Re-export module for the Zilog Z80 processor.
module Z80
       ( module Z80.Processor
       , module Z80.InstructionSet
       , module Z80.InsnDecode
       , module Z80.Disassembler
       , module Z80.DisasmOutput
       , module Z80.ParseAnalytic
       , module Z80.MisosysEDAS
       , z80processor
       ) where

import Machine

import Z80.Processor
import Z80.InstructionSet
import Z80.Disassembler
import Z80.DisasmOutput
import Z80.ParseAnalytic
import Z80.MisosysEDAS

import Z80.InsnDecode

-- | Constructor function for an emulated Zilog Z80
z80processor :: Z80emulation
z80processor =
  EmulatedProcessor
  { _procPrettyName = "Zilog Z80"
  , _procAliases    = ["z80", "Z80", "Zilog-z80", "Zilog-Z80"]
  , _internals      = z80initialState
  }
