{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( DisasmElement(..)
  , Disassembly(..)
  , Disassembler(..)
  , NullPseudoOp(..)
  , mkInitialDisassembly
  ) where

import Data.Label
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as DS
import Data.Vector.Unboxed (Vector)
import Data.ByteString (ByteString)

-- | 'DisasmElement' is a dissassembly element: a disassembled instruction (with corresponding address and instruction
-- words) or pseudo operation.
data DisasmElement addrType wordType instructionType pseudoOptype where
  DisasmInst :: addrType
             -> Vector wordType
             -> instructionType
             -> DisasmElement addrType wordType instructionType pseudoOpType
  DisasmPseudo :: pseudoOpType
               -> DisasmElement addrType wordType instructionType pseudoOpType
  
-- | A 'Disassembly' encapsulates the disassembler state. 
data Disassembly addrType wordType instructionType pseudoOpType =
  Disassembly
  { _labelNum :: Int                                                                -- ^ A general-purpose label counter, useful
                                                                                    -- for local labels, e.g., "L1", "L2", etc.,
                                                                                    -- that are inserted into the symbol table.
  , _symbolTab :: Map addrType ByteString                                           -- ^ The symbol table for address references in
                                                                                    -- 'disasmSeq'
  , _disasmSeq :: Seq (DisasmElement addrType wordType instructionType pseudoOpType) -- ^ The sequence of tuples, each of which is
                                                                                    -- an address, words corresponding to the
                                                                                    -- disassembled instruction, and the
                                                                                    -- disassembled instruction.

  }

-- |  The 'Disassembler' provides a class for all disassemblers. Note that this
class Disassembler addrType dispType wordType instructionType pseudoOpType where
  disassemble :: Vector wordType                -- ^ Vector of words to disassemble
              -> addrType                       -- ^ Origin (base address) of the disassembly
              -> addrType                       -- ^ Start address, relative to the origin, where disassembly starts
              -> dispType                       -- ^ Number of words to disassemble (may be a signed type)
              -> Disassembly addrType wordType instructionType pseudoOpType -- ^ The initial list of disassembled instructions, to
                                                                            -- which results will be appended
              -> Disassembly addrType wordType instructionType pseudoOpType -- ^ The resulting disassembly sequence

-- | The null/empty pseudo operation. This is primarily useful for testing or in cases where the disassembled output is simply
-- binary.
data NullPseudoOp = NullPseudoOp

-- Emit Template Haskell splices for lenses
mkLabels [ ''Disassembly ]

-- | Make an initial 'Disassembly' record
mkInitialDisassembly :: Disassembly addrType wordType instructionType pseudoOpType
mkInitialDisassembly = Disassembly 0 Map.empty DS.empty