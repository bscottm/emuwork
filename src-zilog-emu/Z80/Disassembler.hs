{-# LANGUAGE DeriveDataTypeable #-}

-- | The Z80 disassembler module
module Z80.Disassembler
  ( -- * Types
    Z80DisasmElt
  , Z80PseudoOps(..)
  , Z80disassembly(..)

    -- * Functions
  , isZ80AddrIns
  , z80InsAddr
  , z80InsLength
  , z80disbytes
  , z80disasciiz
  , z80disascii
  , mkInitialDisassembly
  , z80DefaultPostProcessor

  -- * Lens functions for 'Z80disassembly'
  , labelNum
  , addrInDisasmRange
  , symbolTab
  , disasmSeq
  ) where

import           Control.Lens hiding ((|>))
import           Data.Data
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
-- import           Debug.Trace
import qualified Data.Vector.Unboxed as DVU
import           Data.Word

import           Machine
import           Z80.Processor
import           Z80.InstructionSet
import           Z80.InsnDecode()

-- | Disassembly elements for the Z80
type Z80DisasmElt = DisElement Z80instruction Z80addr Z80word Z80PseudoOps

-- | Pseudo disassembler operations: These are elements such as bytes to dump, various types of strings, etc.
data Z80PseudoOps where
  -- Byte from an arbitrary expression
  ByteExpression :: Z80addr
                 -> T.Text
                 -> Word8
                 -> Z80PseudoOps
  deriving (Typeable, Data)

-- | Z80 instruction or pseudo operation contains an address?
isZ80AddrIns :: Z80DisasmElt
             -> Bool
isZ80AddrIns (ExtPseudo (ByteExpression _ _ _)) = True
isZ80AddrIns elt                                = disEltHasAddr elt

-- | Extract address component from a Z80 disassembler element
z80InsAddr :: Z80DisasmElt
           -> Z80addr
z80InsAddr (ExtPseudo (ByteExpression addr _ _)) = addr
z80InsAddr elt                                   = disEltGetAddr elt

-- | Get the instruction or pseudo operation's length
z80InsLength :: Z80DisasmElt
             -> Int
z80InsLength (ExtPseudo (ByteExpression _ _ _)) = 1
z80InsLength elt                                = disEltGetLength elt

-- | Disassembler state, which indexes the 'Disassembly' type family.
data Z80disassembly =
  Z80disassembly
  { -- | A general-purpose label counter, useful for local labels, e.g., "L1", "L2", etc., that are inserted
    -- into the symbol table.
    _labelNum :: Int
    -- | Predicate: Is the address in the disassembler's range? Note that the default function always returns 'True'.
  , _addrInDisasmRange :: Z80addr               -- The address to test
                       -> Bool                  -- 'True' if in disassembler's range, 'False' otherwise.
    -- | The symbol table mapping between addresses and symbol names in 'disasmSeq'
  , _symbolTab :: HashMap Z80addr  T.Text
    -- | The sequence of tuples, each of which is an address, words corresponding to the disassembled instruction, and the
    -- disassembled instruction.
  , _disasmSeq :: Seq Z80DisasmElt
  }

-- Emit the Template Haskell hair for lenses:
makeLenses ''Z80disassembly

-- | Make an initial 'Z80disassembly' record
mkInitialDisassembly :: Z80disassembly
mkInitialDisassembly = Z80disassembly
                       { _labelNum           = 0
                       , _addrInDisasmRange  = (\_ -> True)
                       , _symbolTab          = H.empty
                       , _disasmSeq          = Seq.empty
                       }

-- | Where the real work of the Z80 disassembly happens...
disasm :: Z80disassembly                        -- ^ Incoming disassembly sequence and state
       -> EmulatedSystem procInternals Z80addr Z80word Z80instruction
                                                -- ^ The emulated Z80 system
       -> Z80PC                                 -- ^ Current program counter
       -> Z80PC                                 -- ^ The disassembly's last address
       -> ( Z80DisasmElt
            -> Z80memory
            -> Z80PC
            -> Z80disassembly
            -> (Z80PC, Z80disassembly)
          )                                     -- ^ Post-processing function
       -> Z80disassembly                        -- ^ Resulting disassmbly sequence and state
disasm dstate theSystem thePC lastpc postProc = disasm' thePC dstate
  where
    theMem        = theSystem ^. memory
    addrInDisasmF = dstate    ^. addrInDisasmRange

    disasm' pc curDState
      --   | trace ("disasm " ++ (show pc)) False = undefined
      | pc <= lastpc =
        let DecodedInsn newpc insn = idecode pc theMem
            -- Identify symbols where absolute addresses are found and build up a symbol table for a later
            -- symbol translation pass:
            curDState'             =
              case insn of
                -- Somewhat dubious:
                -- LD (RPair16ImmLoad _rp (AbsAddr addr))  -> collectSymtab curDState addr "M"
                LD (HLIndirectStore (AbsAddr addr))     -> collectSymtab curDState addr "M"
                LD (HLIndirectLoad  (AbsAddr addr))     -> collectSymtab curDState addr "M"
                LD (RPIndirectLoad _rp (AbsAddr addr))  -> collectSymtab curDState addr "M"
                LD (RPIndirectStore _rp (AbsAddr addr)) -> collectSymtab curDState addr "M"
                DJNZ (AbsAddr addr)                     -> collectSymtab curDState addr "L"
                JR (AbsAddr addr)                       -> collectSymtab curDState addr "L"
                JRCC _cc (AbsAddr addr)                 -> collectSymtab curDState addr "L"
                JP (AbsAddr addr)                       -> collectSymtab curDState addr "L"
                JPCC _cc (AbsAddr addr)                 -> collectSymtab curDState addr "L"
                CALL (AbsAddr addr)                     -> collectSymtab curDState addr "SUB"
                CALLCC _cc (AbsAddr addr)               -> collectSymtab curDState addr "SUB"
                _otherwise                              -> curDState
            (newpc', curDState'')  = postProc (mkDisasmInst pc newpc theMem insn) theMem newpc curDState'
        in  disasm' newpc' curDState''
      | otherwise = curDState

    mkDisasmInst :: Z80PC -> Z80PC -> Z80memory -> Z80instruction -> Z80DisasmElt
    mkDisasmInst (PC oldpc) (PC newpc) (MemorySystem msys) ins =
      let opcodes = mFetchN msys oldpc (fromIntegral (newpc - oldpc))
          cmnt
            | LD (RPair16ImmLoad _rp (AbsAddr addr)) <- ins, addrInDisasmF addr = "INTREF"
            | otherwise                                                         = T.empty
      in  mkDisasmInsn oldpc opcodes ins cmnt
    -- Probe the symbol table for 'destAddr', add a new symbol for this address, if in the range of the disassembled
    -- memory
    collectSymtab curDState destAddr prefix =
      let symTab       = curDState ^. symbolTab
          addrInRangeF = curDState ^. addrInDisasmRange
          isInSymtab   = destAddr `H.member` symTab
          label        = T.append prefix (curDState ^. labelNum & (T.pack . show))
      in  if (addrInRangeF destAddr) && not isInSymtab then
            (symbolTab %~ (H.insert destAddr label)) . (labelNum +~ 1) $ curDState
          else
            curDState

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disbytes :: Z80disassembly                   -- ^ Current disassembly state
            -> Z80memory                        -- ^ Vector of bytes from which to extract some data
            -> Z80PC                            -- ^ Start address from which to grab bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Z80disassembly                   -- ^ Resulting diassembly state
z80disbytes dstate (MemorySystem mem) (PC sAddr) nBytes =
  disasmSeq %~ (|> (mkByteRange sAddr $ mFetchN mem sAddr (fromIntegral nBytes))) $ dstate

-- | Grab (what is presumably) an ASCII string sequence, terminating at the first 0 encountered. This is somewhat inefficient
-- because multiple 'Vector' slices get created.
z80disasciiz :: Z80disassembly                  -- ^ Current disassembly state
             -> Z80memory                       -- ^ Vector of bytes from which to extract some data
             -> Z80PC                           -- ^ Start address
             -> Z80disassembly                  -- ^ Resulting diassembly state
z80disasciiz dstate (MemorySystem mem) (PC sAddr) =
  let sRange       = maxBound - sAddr
      toSearch     = mFetchN mem sAddr (fromIntegral sRange)
      foundStr idx = mkAsciiZ sAddr (DVU.slice 0 (idx + 1) toSearch)
  in  case DVU.elemIndex 0 toSearch of
        Nothing  -> dstate                      -- Not found?
        Just idx -> disasmSeq %~ (|> foundStr idx) $ dstate

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disascii :: Z80disassembly                   -- ^ Current disassembly state
            -> Z80memory                        -- ^ Vector of bytes from which to extract some data
            -> Z80PC                            -- ^ Start address from which to start extracting bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Z80disassembly                   -- ^ Resulting diassembly state
z80disascii dstate (MemorySystem mem) (PC sAddr) nBytes =
  disasmSeq %~ (|> (mkAscii sAddr $ mFetchN mem sAddr (fromIntegral nBytes))) $ dstate

-- | Z80 default instruction post processor. This merely appends the decoded instruction onto the disassembly sequence.
z80DefaultPostProcessor :: Z80DisasmElt
                        -> Z80memory
                        -> Z80PC
                        -> Z80disassembly
                        -> (Z80PC, Z80disassembly)
z80DefaultPostProcessor elt _mem pc z80dstate = (pc, disasmSeq %~ (|> elt) $ z80dstate)

-- | 'Disassembler' type family instance for the Z80's disassembler
instance Disassembler Z80disassembly Z80instruction Z80addr Z80word Z80PseudoOps where
  disassemble = disasm
