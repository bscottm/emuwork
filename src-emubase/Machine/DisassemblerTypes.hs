{-# LANGUAGE DeriveDataTypeable #-}

-- | Basic types for all machine disassemblers
module Machine.DisassemblerTypes
  ( -- * Types
    DisasmState(..)
  , DisElement(..)
  , DisEltAddress(..)
  , NullPseudoOp(..)
  , DisElementPostProc

    -- * Functions
  , disassembler
  , mkDisassemblyState
  , defaultPostProcessor
  , disasmMRead
  , disasmMReadN
  , mkDisasmInsn
  , mkDisOrigin
  , mkByteRange
  , mkAddr
  , mkAsciiZ
  , mkAscii
  , mkEquate
  , mkLineComment
  , mkExtPseudo
  , disEltAddress
  , disEltLabel
  , mkPlainAddress
  , mkLabeledAddress
  , disEltHasAddr
  , disEltGetAddr
  , disEltGetLength

    -- * Lenses
  , disasmSystem
  , disasmLabelNum
  , disasmCurAddr
  , disasmCurAddr'
  , disasmFinishAddr
  , disasmOriginAddr
  , disasmEndAddr
  , disasmEndAddr'
  , disasmSymbolTable
  , disasmPostProc
  ) where

import           Control.Arrow          (second, (>>>))
#if MIN_VERSION_microlens_platform(0,4,10)
import           Lens.Micro.Platform    (Lens', (&), (.~), (^.), (+~))
#else
import           Lens.Micro.Platform    (Lens', (&), (.~), (^.), ASetter, over)
#endif
import           Data.Data
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as H
import           Data.Sequence          (Seq, (><))
import qualified Data.Sequence          as Seq
import qualified Data.Text              as T
import           Data.Vector.Unboxed    (Unbox, Vector)
import qualified Data.Vector.Unboxed    as DVU
import           Text.Printf

import           Machine.ProgramCounter
import           Machine.System
import           Machine.Utils

-- import Debug.Trace

-- | The core of the disassembly machine:
disassembler :: (Ord addrType
                , Integral addrType
                , Integral wordType
                , Unbox wordType
                -- The things you need for debugging. :-)
                , PrintfArg addrType
                , PrintfArg wordType
                , ShowHex addrType
                , ShowHex wordType
                , Show addrType
                , Show wordType
                , Show insnType
                , Show extPseudoType
                )
             => DisasmState cpuType insnType addrType wordType extPseudoType
             -- ^ Initial disassembly state
             -> (Seq (DisElement insnType addrType wordType extPseudoType)
                , DisasmState cpuType insnType addrType wordType extPseudoType
                )
             -- ^ The resulting disassembly sequence and disassembler state
disassembler = disassembler' (Seq.empty ><)
  where
    disassembler' dseq dstate
      | curPC <- dstate ^. disasmCurAddr
      , curPC < dstate ^. disasmFinishAddr
      = let sys                   = dstate ^. disasmSystem
            decoder               = sysGetIDecoder sys
            (seqDisasm', dstate') = second (updateSystem dstate) >>> mkDisElt curPC >>> doPostProc $ decoder curPC sys
        in  disassembler' (dseq seqDisasm' ><) dstate'
      | otherwise
      = (dseq Seq.empty, dstate)
      where
        updateSystem dstate' sys        = dstate' & disasmSystem .~ sys
        doPostProc (dstate', disElt)    = (dstate' ^. disasmPostProc) disElt dstate'
        mkDisElt curPC (insn', dstate') = ( dstate' & disasmSystem .~ sys'
                                                    & disasmCurAddr .~ newPC
                                          , mkDisasmInsn (unPC curPC) insMem (insn' ^. decodedInsn) T.empty
                                          )
          where
            newPC = insn'  ^. decodedInsnPC
            (insMem, sys') = sysMReadN (unPC curPC) (fromIntegral (newPC - curPC)) (dstate' ^. disasmSystem)

-- | 'DisElement' is a dissassembly element: a disassembled instruction (with corresponding address and instruction
-- words) or pseudo operation.
data DisElement insnType addrType wordType extPseudoType =
  -- Disassembled instruction
    DisasmInsn (DisEltAddress addrType)        -- Instruction address
                (Vector wordType)              -- Vector of words corresponding to the instruction, e.g. opcodes
                insnType                       -- The instruction
                T.Text                         -- Optional comment
  -- Disassembly origin
  | DisOrigin   (DisEltAddress addrType)
  -- Sequence of bytes
  | ByteRange   (DisEltAddress addrType)         -- Start address
                (Vector wordType)                -- Bytes
  -- An (sybolic|absolute) address
  | Addr        (DisEltAddress addrType)         -- Adress of where this address is stored
                (SymAbsAddr addrType)            -- The address to be annotated
                (Vector wordType)                -- The actual address bytes
  -- 0-terminated string (yes, these were used back in the pre-C days...)
  | AsciiZ      (DisEltAddress addrType)         -- Start of string
                (Vector wordType)                -- The string, not including the zero terminator
  -- Simple ASCII string
  | Ascii       (DisEltAddress addrType)
                (Vector wordType)
  -- Address equation: associates a symbol with an address of something, which is also added to the
  -- disassembler's symbol table
  | Equate      T.Text
                (DisEltAddress addrType)
  -- Comment, printed as a line, as opposed to after an mnemonic and operands
  | LineComment T.Text
  -- Extensions to the "standard" disassembler pseudo instructions
  | ExtPseudo   extPseudoType
  deriving (Data, Typeable, Show)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Make a new 'Disasm'. The optional address label is set to empty.
mkDisasmInsn  :: addrType                       -- ^ Instruction address
              -> Vector wordType                -- ^ Vector of words corresponding to the instruction, e.g. opcodes
              -> insnType                       -- ^ The instruction
              -> T.Text                         -- ^ Optional comment
              -> DisElement insnType addrType wordType extPseudoType
mkDisasmInsn addr = DisasmInsn (mkPlainAddress addr)

mkDisOrigin   :: addrType
              -> DisElement insnType addrType wordType extPseudoType
mkDisOrigin   = DisOrigin . mkPlainAddress

-- | Make a new 'ByteRange'. The optional address label is set to empty.
mkByteRange   :: addrType
              -- ^ Start address
              -> Vector wordType
              -- ^ Bytes
              -> DisElement insnType addrType wordType extPseudoType
mkByteRange = ByteRange . mkPlainAddress

-- | Make a new 'Addr'. The optional address label is set to empty.
mkAddr        :: addrType                       -- Adress of where this address is stored
              -> SymAbsAddr addrType            -- The address to be annotated
              -> Vector wordType                -- The actual address bytes
              -> DisElement insnType addrType wordType extPseudoType
mkAddr        = Addr . mkPlainAddress

-- | Make a new 'AsciiZ'. The optional address label is set to empty.
mkAsciiZ      :: addrType                       -- Start of string
              -> Vector wordType                -- The string, not including the zero terminator
              -> DisElement insnType addrType wordType extPseudoType
mkAsciiZ      = AsciiZ . mkPlainAddress

-- | Make a new 'Ascii'. The optional address label is set to empty.
mkAscii       :: addrType
              -> Vector wordType
              -> DisElement insnType addrType wordType extPseudoType
mkAscii       = Ascii . mkPlainAddress

-- | Make a new 'Equate' for the disassembler's symbl table.
mkEquate      :: T.Text
              -> addrType
              -> DisElement insnType addrType wordType extPseudoType
mkEquate sym addr = Equate sym (Plain addr)

-- | Make a new 'LineComment'
mkLineComment :: T.Text
              -> DisElement insnType addrType wordType extPseudoType
mkLineComment = LineComment

-- | Make a new 'ExtPseudo'
mkExtPseudo   :: extPseudoType
              -> DisElement insnType addrType wordType extPseudoType
mkExtPseudo = ExtPseudo

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassembly element's address, with optional label
data DisEltAddress addrType =
    Plain addrType
  | Labeled addrType T.Text
  deriving (Eq, Show, Data, Typeable)

instance (ShowHex addrType)
         => ShowHex (DisEltAddress addrType) where
  asHex (Plain addr)     = asHex addr
  asHex (Labeled addr _) = asHex addr

-- | Extract the address from a disassembler element address type
disEltAddress :: DisEltAddress addrType
              -> addrType
disEltAddress (Plain addr)          = addr
disEltAddress (Labeled addr _label) = addr

-- | Extract the label from a disassembler element address. Returns empty if a 'Plain' element address
disEltLabel :: DisEltAddress addrType
            -> T.Text
disEltLabel (Plain _addr)             = T.empty
disEltLabel (Labeled _addr addrLabel) = addrLabel

-- | Make a 'Plain' disassembler element address
mkPlainAddress :: addrType
               -> DisEltAddress addrType
mkPlainAddress = Plain

-- | Make a 'Labeled' disassembler element address
mkLabeledAddress :: addrType
                 -> T.Text
                 -> DisEltAddress addrType
mkLabeledAddress = Labeled

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Disassembler state needed to generically support instruction disassembly
data DisasmState cpuType insnType addrType wordType extPseudoType =
  DisasmState
    { _disasmSystem :: EmulatedSystem cpuType insnType addrType wordType
      -- ^ The system whose memory is being accessed and disassembled
    , _disasmLabelNum    :: Int
    , _disasmCurAddr     :: ProgramCounter addrType
      -- ^ Current address being disassembled, initialized to the starting address
    , _disasmFinishAddr  :: ProgramCounter addrType
      -- ^ Disassembly finish address. Note that this may not be equal to the end address.
    , _disasmOriginAddr  :: ProgramCounter addrType
      -- ^ Origin (starting) address
    , _disasmEndAddr     :: ProgramCounter addrType
      -- ^ Ending (limit) address
    , _disasmSymbolTable :: HashMap addrType T.Text
      -- ^ Symbol table: Addresses mapped to text
    , _disasmPostProc    :: DisElementPostProc cpuType insnType addrType wordType extPseudoType
      -- ^ Post-processing function for system-specific specials. For example, the TRS-80 Model I Level 2 ROM
      -- uses the "RST 08" instruction to compare the accumulator with a character following the instruction.
      -- The post-processing function looks for "RST 08" instructions and subsequently moves the '_curAddr'
      -- forward to collect the byte.
    }


-- | 'DisElement' post-processing function
type DisElementPostProc cpuType insnType addrType wordType extPseudoType =
  DisElement insnType addrType wordType extPseudoType
  -- ^ Decoded instruction or pseudo-operation
  -> DisasmState cpuType insnType addrType wordType extPseudoType
  -- Incoming disassembly state
  -> ( Seq (DisElement insnType addrType wordType extPseudoType)
     , DisasmState cpuType insnType addrType wordType extPseudoType
     )
  -- ^ Resulting disassembly state and disassembly element sequence


disasmSystem :: Lens' (DisasmState cpuType insnType addrType wordType extPseudoType)
                      (EmulatedSystem cpuType insnType addrType wordType)
disasmSystem  f ds = (\sys' -> ds { _disasmSystem = sys' }) <$> f (_disasmSystem ds)

disasmLabelNum :: Lens' (DisasmState cpuType insnType addrType wordType extPseudoType) Int
disasmLabelNum f ds = (\num -> ds { _disasmLabelNum = num }) <$> f (_disasmLabelNum ds)

disasmCurAddr :: Lens' (DisasmState cpuType insnType addrType wordType extPseudoType) (ProgramCounter addrType)
disasmCurAddr f ds = (\addr -> ds { _disasmCurAddr = addr }) <$> f (_disasmCurAddr ds)

-- | Alternate version that 'unPC'-s the program counter
disasmCurAddr' :: DisasmState cpuType insnType addrType wordType extPseudoType
               -> addrType
disasmCurAddr' ds = ds ^. disasmCurAddr & unPC

disasmFinishAddr :: Lens' (DisasmState cpuType insnType addrType wordType extPseudoType) (ProgramCounter addrType)
disasmFinishAddr f ds = (\addr -> ds { _disasmFinishAddr = addr }) <$> f (_disasmFinishAddr ds)

disasmOriginAddr ::Lens' (DisasmState cpuType insnType addrType wordType extPseudoType) (ProgramCounter addrType)
disasmOriginAddr f ds = (\addr -> ds { _disasmOriginAddr = addr }) <$> f (_disasmOriginAddr ds)

disasmEndAddr :: Lens' (DisasmState cpuType insnType addrType wordType extPseudoType) (ProgramCounter addrType)
disasmEndAddr f ds = (\addr -> ds { _disasmEndAddr = addr }) <$> f (_disasmEndAddr ds)
--
-- | Alternate version that 'unPC'-s the program counter
disasmEndAddr' :: DisasmState cpuType insnType addrType wordType extPseudoType
               -> addrType
disasmEndAddr' ds = ds ^. disasmEndAddr & unPC

disasmSymbolTable :: Lens' (DisasmState cpuType insnType addrType wordType extPseudoType) (HashMap addrType T.Text)
disasmSymbolTable f ds = (\syms -> ds { _disasmSymbolTable = syms }) <$> f (_disasmSymbolTable ds)

disasmPostProc :: Lens' (DisasmState cpuType insnType addrType wordType extPseudoType)
                        (DisElementPostProc cpuType insnType addrType wordType extPseudoType)
disasmPostProc f ds = (\postProc -> ds { _disasmPostProc = postProc }) <$> f (_disasmPostProc ds)

-- | Create an initial disassembly state for a given emulated system. (Note: There is no good default for the emulated
-- system, otherwise, this would be a good candidate for a 'Monoid' instance.)
mkDisassemblyState :: Num addrType
                   => EmulatedSystem cpuType insnType addrType wordType
                   -> addrType
                   -> addrType
                   -> DisasmState cpuType insnType addrType wordType extPseudoType
mkDisassemblyState sys sAddr eAddr =
  DisasmState {
    _disasmSystem      = sys
  , _disasmLabelNum    = 1
  , _disasmCurAddr     = PC sAddr
  , _disasmFinishAddr  = PC eAddr
  , _disasmOriginAddr  = PC sAddr
  , _disasmEndAddr     = PC eAddr
  , _disasmSymbolTable = H.empty
  , _disasmPostProc    = defaultPostProcessor
  }

-- | A default (null) post processor. It just returns a sequence singleton with the disassembly element and the
-- current disassembly state.
defaultPostProcessor :: DisElementPostProc cpuType insnType addrType wordType extPseudoType
defaultPostProcessor elt dstate = (Seq.singleton elt, dstate)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | The null/empty pseudo operation. This is primarily useful for testing or in cases where the disassembled output is simply
-- binary.
data NullPseudoOp = NullPseudoOp

-- | Does the instruction or pseudo-operation contain an address? Primarily useful for filtering a sequence
-- when checking for continuity and all one wants are instructions and pseudo-operations with addresses (e.g., no
-- comments or equates.)
disEltHasAddr :: DisElement insnType addrType wordType extPseudoType
              -> Bool
disEltHasAddr DisasmInsn{}    = True
disEltHasAddr (ByteRange _ _) = True
disEltHasAddr Addr{}          = True
disEltHasAddr (AsciiZ _ _)    = True
disEltHasAddr (Ascii _ _)     = True
disEltHasAddr _               = False

-- | Extract an address from an instruction or pseudo-operation.
disEltGetAddr :: DisElement insnType addrType wordType extPseudoType
              -> addrType
disEltGetAddr (DisasmInsn addr _ _ _) = disEltAddress addr
disEltGetAddr (ByteRange addr _)      = disEltAddress addr
disEltGetAddr (Addr addr _ _)         = disEltAddress addr
disEltGetAddr (AsciiZ addr _)         = disEltAddress addr
disEltGetAddr (Ascii addr _)          = disEltAddress addr
disEltGetAddr _                       = undefined

-- | Extract length from an instruction or pseudo-operation.
disEltGetLength :: ( Unbox wordType ) =>
                   DisElement insnType addrType wordType extPseudoType
                -> Int
disEltGetLength (DisasmInsn _ bytes _ _) = DVU.length bytes
disEltGetLength (ByteRange _ bytes)      = DVU.length bytes
disEltGetLength Addr{}                   = 2
disEltGetLength (AsciiZ _ bytes)         = DVU.length bytes
disEltGetLength (Ascii _ bytes)          = DVU.length bytes
disEltGetLength _                        = 0

-- | Read a word from a system's memory at the disassembler's current address , returning the word and updated
-- the disassembly state
disasmMRead :: ( Integral addrType
               , Integral wordType
               , DVU.Unbox wordType
               )
             => DisasmState cpuType insnType addrType wordType extPseudoType
             -> (wordType, DisasmState cpuType insnType addrType wordType extPseudoType)
disasmMRead dstate = (word, dstate & disasmSystem .~ sys' & disasmCurAddr +~ 1)
  where
    (word, sys') = sysMRead (dstate ^. disasmCurAddr & unPC) (dstate ^. disasmSystem)

-- | Read a vector of words from a system's memory at the disassembler's current addresss, returning the vector and
-- the updated disassembly state
disasmMReadN :: ( Integral addrType
                , Integral wordType
                , DVU.Unbox wordType
                , PrintfArg addrType
                , PrintfArg wordType
                , Show addrType
                , Show wordType
                )
             => Int
             -> DisasmState cpuType insnType addrType wordType extPseudoType
             -> (Vector wordType, DisasmState cpuType insnType addrType wordType extPseudoType)
disasmMReadN nWords dstate = (wvec, dstate & disasmSystem .~ sys'& disasmCurAddr +~ fromIntegral nWords)
  where
    (wvec, sys') = sysMReadN (dstate ^. disasmCurAddr & unPC) nWords (dstate ^. disasmSystem)

#if !MIN_VERSION_microlens_platform(0,4,10)
(+~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ n = over l (+ n)
{-# INLINE (+~) #-}
#endif
