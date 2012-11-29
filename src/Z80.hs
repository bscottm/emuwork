{- |
  Re-export module for the Zilog Z80 processor.
-}
module Z80
       ( module Z80.Processor
       , module Z80.CmdDispatch
       , module Z80.InstructionSet
       , module Z80.Disassembler
       , module Z80.DisasmOutput
       , z80processor
       ) where

import Z80.Processor
import Z80.CmdDispatch
import Z80.InstructionSet
import Z80.Disassembler
import Z80.DisasmOutput

import Machine

-- | Constructor function for an emulated Zilog Z80.
z80processor :: EmulatedProcessor
z80processor = EmulatedProcessor
  { machineName = "Zilog Z80"
  , names       = ["z80", "Z80", "Zilog-z80", "Zilog-Z80"]
  , cmdDispatch = z80cmdDispatch
  , internals   = z80initialState
  }
