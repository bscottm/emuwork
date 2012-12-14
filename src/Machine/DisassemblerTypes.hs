-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( Disassembler(..)
  , NullPseudoOp(..)
  ) where

import Machine.EmulatedSystem

-- |  The 'Disassembler' type class and generic interface to disassemblers.
class Disassembler disasmState addrType wordType where
  -- | The main disassembler function
  disassemble :: disasmState                            -- ^ The initial list of disassembled instructions,
                                                        -- to which results will be appended, as well as
                                                        -- the program counter
              -> MemorySystem addrType wordType memSys  -- ^ Vector of words to disassemble
              -> ProgramCounter addrType                -- ^ Starting address to disassemble
              -> ProgramCounter addrType                -- ^ Last address to disassemble
              -> disasmState                            -- ^ The resulting disassembly sequence

-- | The null/empty pseudo operation. This is primarily useful for testing or in cases where the disassembled output is simply
-- binary.
data NullPseudoOp = NullPseudoOp
