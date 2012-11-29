{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( Disassembler(..)
  , NullPseudoOp(..)
  ) where

import Data.Vector.Unboxed (Vector)

-- |  The 'Disassembler' provides an indexed type family for all disassemblers.
class Disassembler disasmState addrType wordType dispType where
  -- | Disassembly state associated with the 'Disassembler' type family. Note that the kind signature implies
  -- that the type family is indexed by addrType, wordType and instructionType. This gives each processor's
  -- disassembler some latitude on how to specify its disassembler internals.
  data Disassembly disasmState
  -- | The main disassembler function
  disassemble :: Vector wordType                -- ^ Vector of words to disassemble
              -> addrType                       -- ^ Origin (base address) of the disassembly
              -> addrType                       -- ^ Start address, relative to the origin, where disassembly starts
              -> dispType                       -- ^ Number of words to disassemble (may be a signed type)
              -> Disassembly disasmState        -- ^ The initial list of disassembled instructions, to which results
                                                -- will be appended
              -> Disassembly disasmState        -- ^ The resulting disassembly sequence

-- | The null/empty pseudo operation. This is primarily useful for testing or in cases where the disassembled output is simply
-- binary.
data NullPseudoOp = NullPseudoOp
