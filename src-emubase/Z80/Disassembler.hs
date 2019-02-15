{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}

-- | The Z80 disassembler module
module Z80.Disassembler
  ( -- * Types
    Z80DisasmElt
  , Z80PseudoOps(..)
  , Z80disassembly

    -- * Functions
  , isZ80AddrIns
  , z80InsAddr
  , z80InsLength
  , z80disbytes
  , z80disasciiz
  , z80disascii
  , z80DefaultPostProcessor
  )
where

import           Control.Lens                   ( views
                                                , (%~)
                                                , (&)
                                                , (+~)
                                                , (.~)
                                                , (^.)
                                                )
import           Control.Arrow                  ( first )
import qualified Data.Char                     as C
import           Data.Data
import qualified Data.HashMap.Strict           as H
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import           Data.Vector.Unboxed           ((!))
import qualified Data.Vector.Unboxed           as DVU
import           Data.Word

import           Machine
import           Z80.InsnDecode                 ( )
import           Z80.InstructionSet
import           Z80.Processor

-- import           Debug.Trace

-- | Disassembly state for generic Z80s
type Z80disassembly = DisasmState Z80state Z80instruction Z80addr Z80word Z80PseudoOps

-- | Disassembly elements for the Z80
type Z80DisasmElt = DisElement Z80instruction Z80addr Z80word Z80PseudoOps

-- | Pseudo disassembler operations: These are elements such as bytes to dump, various types of strings, etc.
data Z80PseudoOps where
  -- Byte from an arbitrary expression
  ByteExpression ::Z80addr
                 -> T.Text
                 -> Word8
                 -> Z80PseudoOps
  deriving (Typeable, Data, Show)

-- | Z80 instruction or pseudo operation contains an address?
isZ80AddrIns :: Z80DisasmElt -> Bool
isZ80AddrIns (ExtPseudo ByteExpression{}) = True
isZ80AddrIns elt                          = disEltHasAddr elt

-- | Extract address component from a Z80 disassembler element
z80InsAddr :: Z80DisasmElt -> Z80addr
z80InsAddr (ExtPseudo (ByteExpression addr _ _)) = addr
z80InsAddr elt = disEltGetAddr elt

-- | Get the instruction or pseudo operation's length
z80InsLength :: Z80DisasmElt -> Int
z80InsLength (ExtPseudo ByteExpression{}) = 1
z80InsLength elt                          = disEltGetLength elt

-- | Grab a sequence of bytes from memory, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disbytes
  :: Z80disp
            -- ^ Current disassembly state
  -> Z80disassembly
            -- ^ Number of bytes to extract
  -> (Z80DisasmElt, Z80disassembly)
            -- ^ Resulting diassembly state
z80disbytes nBytes dstate =
  let sAddr = views disasmCurAddr unPC dstate in first (mkByteRange sAddr) $ disasmMReadN (fromIntegral nBytes) dstate

-- | Grab (what is presumably) an ASCII string sequence, terminating at the first 0 encountered. This is somewhat inefficient
-- because multiple 'Vector' slices get created.
z80disasciiz
  :: Z80disassembly
             -- ^ Current disassembly state
  -> (Seq.Seq Z80DisasmElt, Z80disassembly)
             -- ^ Resulting diassembly state
z80disasciiz dstate =
  -- FIXME: Don't search the entire contents of memory...
  let sAddr            = views disasmCurAddr unPC dstate
      sRange           = maxBound - sAddr
      (toSearch, sys') = sysMReadN sAddr (fromIntegral sRange) (dstate ^. disasmSystem)
      makeSeq disSeq bytes offs
        | DVU.null bytes
        = disSeq
        | (C.isPrint . C.chr . fromIntegral) (bytes ! 0)
        = let printBytes = DVU.takeWhile (C.isPrint . C.chr . fromIntegral) bytes
              asciiZAsm  = mkAsciiZ (sAddr + offs) printBytes
          in  makeSeq (disSeq |> asciiZAsm)
                      (DVU.drop (DVU.length printBytes) bytes)
                      (offs + fromIntegral (DVU.length printBytes))
        | (not . C.isPrint . C.chr . fromIntegral) (bytes ! 0)
        = let notprintBytes = DVU.takeWhile (not . C.isPrint . C.chr . fromIntegral) bytes
              bytesAsm      = mkByteRange (sAddr + offs) notprintBytes
          in  makeSeq (disSeq |> bytesAsm)
                      (DVU.drop (DVU.length notprintBytes) bytes)
                      (offs + fromIntegral (DVU.length notprintBytes))
        | otherwise
        = undefined
  in  case DVU.elemIndex 0 toSearch of
        Nothing  -> (Seq.singleton $ LineComment "BAD/FAULTY Z80 ASCIIZ: no zero terminator found.", dstate & disasmSystem .~ sys')
        Just idx -> (makeSeq Seq.empty (DVU.slice 0 (idx + 1) toSearch) 0
                    , dstate & disasmSystem .~ sys' & disasmCurAddr +~ fromIntegral idx + 1
                    )

-- | Grab a sequence of bytes from the memory image, returning an 'Ascii' disassmbly element
z80disascii
  :: Z80disp
            -- ^ Current disassembly state
  -> Z80disassembly
            -- ^ Number of bytes to extract
  -> (Z80DisasmElt, Z80disassembly)
            -- ^ Resulting diassembly state
z80disascii nBytes dstate =
  let sAddr = views disasmCurAddr unPC dstate in first (mkAscii sAddr) $ disasmMReadN (fromIntegral nBytes) dstate

-- | Z80 default instruction post processor. This merely appends the decoded instruction onto the disassembly sequence.
z80DefaultPostProcessor :: DisElementPostProc Z80state Z80instruction Z80addr Z80word Z80PseudoOps
z80DefaultPostProcessor disElt@DisasmInsn{} dstate = ((Seq.singleton . internalAddrRef) disElt, labelAddresses disElt dstate)
 where
  inrange addr = views disasmOriginAddr unPC dstate <= addr && addr <= views disasmEndAddr unPC dstate
  internalAddrRef disElt'@(DisasmInsn disAddr bytes insn _cmnt)
    | LD (RPair16ImmLoad _rp (AbsAddr addr)) <- insn
    -- 0x0 tends to be a constant, so if there's an address equate, don't substitute or point out an internal reference.
                                                    , inrange addr && 0 < addr
    = (DisasmInsn disAddr bytes insn "poss. internal ref")
    | otherwise
    = disElt'
  internalAddrRef disElt' = disElt'

  labelAddresses (DisasmInsn _addr _bytes insn _cmnt) dstate'
    | Just (prefix, destAddr) <- hasAddress, inrange destAddr && not (views disasmSymbolTable (destAddr `H.member`) dstate')
    = dstate' & disasmSymbolTable %~ H.insert destAddr (mkLabel prefix) & disasmLabelNum +~ 1
    | otherwise
    = dstate'
   where
    hasAddress = case insn of
        -- Somewhat dubious:
        -- LD (RPair16ImmLoad _rp (AbsAddr addr))  -> doCollectSymtab "M"
      LD (HLIndirectStore (AbsAddr addr)) -> Just ("M", addr)
      LD (HLIndirectLoad (AbsAddr addr)) -> Just ("M", addr)
      LD (RPIndirectLoad _rp (AbsAddr addr)) -> Just ("M", addr)
      LD (RPIndirectStore _rp (AbsAddr addr)) -> Just ("M", addr)
      DJNZ (AbsAddr addr)       -> Just ("L", addr)
      JR   (AbsAddr addr)       -> Just ("L", addr)
      JRCC _cc (AbsAddr addr)   -> Just ("L", addr)
      JP (AbsAddr addr)         -> Just ("L", addr)
      JPCC _cc (AbsAddr addr)   -> Just ("L", addr)
      CALL (AbsAddr addr)       -> Just ("SUB", addr)
      CALLCC _cc (AbsAddr addr) -> Just ("SUB", addr)
      _otherwise                -> Nothing
    mkLabel prefix = T.append prefix (views disasmLabelNum (T.pack . show) dstate')
  labelAddresses _disElt dstate' = dstate'
-- Everything else...
z80DefaultPostProcessor disElt dstate' = defaultPostProcessor disElt dstate'
