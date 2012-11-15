{-# LANGUAGE RankNTypes #-}
-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( DisasmElement(..)
  , Disassembly
  , Disassembler(..)
  , NullPseudoOp
  ) where

import Data.Sequence (Seq)
import Data.Vector.Unboxed (Vector)

data DisasmElement addrType wordType instructionType pseudoOptype where
  DisasmInst :: addrType
             -> Vector wordType
             -> instructionType
             -> DisasmElement addrType wordType instructionType pseudoOpType
  DisasmPseudo :: pseudoOpType
               -> DisasmElement addrType wordType instructionType pseudoOpType
  
-- | A 'Disassembly' produces sequence of tuples, where each tuple corresponds to addresses, words corresponding to
-- the disassembled instruction, and the disassembled instruction.
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

data NullPseudoOp = NullPseudoOp