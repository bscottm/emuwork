module Z80.System where

import Lens.Micro.Platform

import Machine.System
import Machine.MemorySystem
import Z80.Processor
import Z80.InstructionSet

-- | Shorthand for the Z80\'s 'EmulatedProcessor' type. This is defined here because 'Z80instruction' is required
-- and would otherwise form a module import cycle.
type Z80emulation      = EmulatedProcessor Z80state Z80instruction Z80addr Z80word

-- | Shorthand for a Z80 emulated system, with a phantom type. The phantom type
-- acts as a discriminant between different kinds of Z80 systems, e.g., the
-- TRS-80 Model I.
type Z80system sysType = EmulatedSystem Z80state Z80instruction Z80addr Z80word

-- | Z80 memory system type
type Z80memory         = MemorySystem Z80addr Z80word

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- Common accessors...
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

-- | Accessor to the register set.
z80registers
 :: Z80system sysType
 -> Z80registers
z80registers z80 = z80 ^. processor . cpu . regs
