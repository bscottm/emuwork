{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( DisasmElement(..)
  , Disassembly(..)
  , Disassembler(..)
  , NullPseudoOp(..)
  ) where

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Vector.Unboxed (Vector)
import Data.ByteString (ByteString)

-- | 
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

data NullPseudoOp = NullPseudoOp

mkLabels [ ''Disassembly ]