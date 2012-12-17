-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( -- * Types
    Disassembler(..)
  , DisElement(..)
  , SymAbsAddr(..)
  , NullPseudoOp(..)
  , DisElementPostProc

    -- * Functions
  , disEltHasAddr
  , disEltGetAddr
  , disEltGetLength
  ) where

import Data.Word
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as DVU
import Data.ByteString.Lazy.Char8 (ByteString)

import Machine.Utils
import Machine.EmulatedSystem

-- | 'DisasmElement' is a dissassembly element: a disassembled instruction (with corresponding address and instruction
-- words) or pseudo operation.
data DisElement insnType addrType wordType extPseudoType where
  -- Disassembled instruction
  DisasmInsn     :: addrType                    -- Instruction address
                 -> Vector wordType             -- Vector of words corresponding to the instruction
                 -> insnType                    -- The instruction
                 -> ByteString                  -- Optional comment
                 -> DisElement insnType addrType wordType extPseudoType
  -- Disassembly origin
  DisOrigin      :: addrType
                 -> DisElement insnType addrType wordType extPseudoType
  -- Sequence of bytes
  ByteRange      :: addrType                    -- Start address
                 -> Vector Word8                -- Bytes
                 -> DisElement insnType addrType wordType extPseudoType
  -- An (sybolic|absolute) address
  Addr           :: addrType                    -- Adress of where this address is stored
                 -> SymAbsAddr addrType         -- The address to be annotated
                 -> Vector Word8                -- The actual address bytes
                 -> DisElement insnType addrType wordType extPseudoType
  -- 0-terminated string (yes, these were used back in the pre-C days...)
  AsciiZ         :: addrType                    -- Start of string
                 -> Vector Word8                -- The string, not including the zero terminator
                 -> DisElement insnType addrType wordType extPseudoType
  -- Simple ASCII string
  Ascii          :: addrType
                 -> Vector Word8
                 -> DisElement insnType addrType wordType extPseudoType
  -- Address equation: associates a symbol with an address of something, which is also added to the
  -- disassembler's symbol table
  Equate         :: ByteString
                 -> addrType
                 -> DisElement insnType addrType wordType extPseudoType
  -- Comment, printed as a line, as opposed to after an mnemonic and operands
  LineComment    :: ByteString
                 -> DisElement insnType addrType wordType extPseudoType
  -- Extensions to the "standard" disassembler pseudo instructions
  ExtPseudo      :: extPseudoType
                 -> DisElement insnType addrType wordType extPseudoType

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | A (symbolic|absolute) address
data SymAbsAddr addrType where
  AbsAddr :: addrType
          -> SymAbsAddr addrType
  SymAddr :: ByteString
          -> SymAbsAddr addrType

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | 'DisElement' post-processing function synonym (but does this really cut down on typing or
-- just make the Haddock output that more readable?)
type DisElementPostProc disasmState insnType memSys addrType wordType extPseudoType =
  ( DisElement insnType addrType wordType extPseudoType  -- Decoded instruction or pseudo-operation
    -> MemorySystem addrType wordType memSys             -- Memory system
    -> ProgramCounter addrType                           -- Current program counter
    -> disasmState                                       -- Incoming disassembly state
    -> (ProgramCounter addrType, disasmState)            -- Resulting disassembly state
  )
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- |  The 'Disassembler' type class and generic interface to disassemblers.
class Disassembler disasmState insnType addrType wordType extPseudoType where
  -- | The main disassembler function
  disassemble :: disasmState                            -- ^ The incoming disassembly state. This data type should collect
                                                        -- the disassembled instruction, associated pseudo-operations.
              -> MemorySystem addrType wordType memSys  -- ^ Memory system from which instructions are disassembled
              -> ProgramCounter addrType                -- ^ Starting address to disassemble
              -> ProgramCounter addrType                -- ^ Last address to disassemble
              -> DisElementPostProc disasmState insnType memSys addrType wordType extPseudoType
	      						-- ^ Post-processing function that is be applied after disasembling 
                                                        -- an instruction. This is useful for situations such as the TRS-80
                                                        -- BASIC ROM, where the \'RST 08\' instruction is always followed by
                                                        -- a byte and two should be emitted together. If no post-processing
                                                        -- is needed, simply pass 'nullPostProcessor' and ignore the parameter
                                                        -- in the actual disassembler.
              -> disasmState                            -- ^ The resulting disassembly sequence

-- | The null/empty pseudo operation. This is primarily useful for testing or in cases where the disassembled output is simply
-- binary.
data NullPseudoOp = NullPseudoOp

-- | Does the instruction or pseudo-operation contain an address? Primarily useful for filtering a sequence
-- when checking for continuity and all one wants are instructions and pseudo-operations with addresses (e.g., no
-- comments or equates.)
disEltHasAddr :: DisElement insnType addrType wordType extPseudoType
              -> Bool
disEltHasAddr (DisasmInsn _ _ _ _) = True
disEltHasAddr (ByteRange _ _)      = True
disEltHasAddr (Addr _ _ _)         = True
disEltHasAddr (AsciiZ _ _)         = True
disEltHasAddr (Ascii _ _)          = True
disEltHasAddr _                    = False

-- | Extract an address from an instruction or pseudo-operation.
disEltGetAddr :: DisElement insnType addrType wordType extPseudoType
              -> addrType
disEltGetAddr (DisasmInsn addr _ _ _) = addr
disEltGetAddr (ByteRange addr _)      = addr
disEltGetAddr (Addr addr _ _)         = addr
disEltGetAddr (AsciiZ addr _)         = addr
disEltGetAddr (Ascii addr _)          = addr
disEltGetAddr _                       = undefined

-- | Extract length from an instruction or pseudo-operation.
disEltGetLength :: ( Unbox wordType ) =>
                   DisElement insnType addrType wordType extPseudoType
                -> Int
disEltGetLength (DisasmInsn _ bytes _ _) = DVU.length bytes
disEltGetLength (ByteRange _ bytes)      = DVU.length bytes
disEltGetLength (Addr _ _ _)             = 2
disEltGetLength (AsciiZ _ bytes)         = DVU.length bytes
disEltGetLength (Ascii _ bytes)          = DVU.length bytes
disEltGetLength _                        = 0

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

instance (ShowHex addrType) =>
         Show (SymAbsAddr addrType) where
  show (AbsAddr addr)  = as0xHexS addr
  show (SymAddr label) = show label
