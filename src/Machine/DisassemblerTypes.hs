-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( DisasmElement(..)
  , Disassembly
  , Disassembler(..)
  , DisasmSymTab
  , NullPseudoOp(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Vector.Unboxed (Vector)

-- | 'DisasmElement' is a dissassembly element: a disassembled instruction (with corresponding address and instruction
-- words) or pseudo operation.
data DisasmElement addrType wordType instructionType pseudoOptype where
  DisasmInst :: addrType
             -> Vector wordType
             -> instructionType
             -> DisasmElement addrType wordType instructionType pseudoOpType
  DisasmPseudo :: pseudoOpType
               -> DisasmElement addrType wordType instructionType pseudoOpType
  
-- | A 'Disassembly' produces sequence of 'DisasmElement' elements.
type Disassembly addrType wordType instructionType pseudoOpType = Seq (DisasmElement addrType wordType instructionType pseudoOpType)

-- |  The 'Disassembler' provides a class for all disassemblers. Note that this
class Disassembler addrType dispType wordType instructionType pseudoOpType where
  disassemble :: Vector wordType                -- ^ Vector of words to disassemble
              -> addrType                       -- ^ Origin (base address) of the disassembly
              -> addrType                       -- ^ Start address, relative to the origin, where disassembly starts
              -> dispType                       -- ^ Number of words to disassemble (may be a signed type)
              -> Disassembly addrType wordType instructionType pseudoOpType -- ^ The initial list of disassembled instructions, to
                                                                            -- which results will be appended
              -> Disassembly addrType wordType instructionType pseudoOpType -- ^ The resulting disassembly sequence

-- | Symbol table for branch destinations, special addresses, etc., used in the disassembled output.
type DisasmSymTab addrType = Map addrType ByteString

-- | The null/empty pseudo operation. This is primarily useful for testing or in cases where the disassembled output is simply
-- binary.
data NullPseudoOp = NullPseudoOp