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
  , z80AddrInDisasmRange
  )
where

import           Lens.Micro.Platform            
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
  let sAddr = disasmCurAddr' dstate in first (mkByteRange sAddr) $ disasmMReadN (fromIntegral nBytes) dstate

-- | Grab (what is presumably) an ASCII string sequence, terminating at the first 0 encountered. This is somewhat inefficient
-- because multiple 'Vector' slices get created.
z80disasciiz
  :: Z80disassembly
             -- ^ Current disassembly state
  -> (Seq.Seq Z80DisasmElt, Z80disassembly)
             -- ^ Resulting diassembly state
z80disasciiz dstate =
  let sAddr            = disasmCurAddr' dstate
      -- findZero :: Z80addr -> Z80disassembly -> DVU.Vector Z80word -> (Z80disassembly, Maybe (DVU.Vector Z80word))
      findZero sAddr' dstate' bytes =
        if sAddr' < disasmEndAddr' dstate'
        then  let nToRead = fromIntegral $ min 16 (disasmEndAddr' dstate' - sAddr)
                  (seg, sys') = sysMReadN sAddr' nToRead (dstate' ^. disasmSystem)
              in  maybe (findZero (sAddr' + 16) (dstate' & disasmSystem .~ sys') (bytes DVU.++ seg))
                        (\idx -> (dstate' & disasmSystem .~ sys', Just $ bytes DVU.++ DVU.slice 0 (idx + 1) seg))
                        (DVU.elemIndex 0 seg)
        else (dstate', Nothing)
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
  in  case findZero sAddr dstate DVU.empty of
        (dstate', Nothing)  -> (Seq.singleton $ LineComment "BAD/FAULTY Z80 ASCIIZ: no zero terminator found.", dstate')
        (dstate', Just bytes) -> (makeSeq Seq.empty bytes 0 , dstate' & disasmCurAddr +~ fromIntegral (DVU.length bytes))

-- | Grab a sequence of bytes from the memory image, returning an 'Ascii' disassmbly element
z80disascii
  :: Z80disp
  -- ^ Current disassembly state
  -> Z80disassembly
  -- ^ Number of bytes to extract
  -> (Z80DisasmElt, Z80disassembly)
  -- ^ Resulting diassembly state
z80disascii nBytes dstate =
  let sAddr = disasmCurAddr' dstate in first (mkAscii sAddr) $ disasmMReadN (fromIntegral nBytes) dstate

-- | Z80 default instruction post processor. This merely appends the decoded instruction onto the disassembly sequence.
z80DefaultPostProcessor :: DisElementPostProc Z80state Z80instruction Z80addr Z80word Z80PseudoOps
z80DefaultPostProcessor disElt@DisasmInsn{} dstate = (Seq.singleton disElt, labelAddresses disElt dstate)
 where
  labelAddresses (DisasmInsn _addr _bytes insn _cmnt) dstate'
    | Just (prefix, destAddr) <- hasAddress, z80AddrInDisasmRange destAddr dstate'
                                             && not (destAddr `H.member` (dstate' ^. disasmSymbolTable))
    = dstate' & disasmSymbolTable %~ H.insert destAddr (mkLabel prefix) & disasmLabelNum +~ 1
    | otherwise
    = dstate'
   where
    hasAddress = case insn of
        -- Somewhat dubious:
        -- LD (RPair16ImmLoad _rp (AbsAddr addr))  -> doCollectSymtab "M"
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
    mkLabel prefix = T.append prefix (T.pack $ show $ dstate' ^. disasmLabelNum)
  labelAddresses _disElt dstate' = dstate'
-- Everything else...
z80DefaultPostProcessor disElt dstate' = defaultPostProcessor disElt dstate'

-- | Commonly used predicate to determine if an address falls within the dissembled start/end.
z80AddrInDisasmRange :: Z80addr
                     -> Z80disassembly
                     -> Bool
z80AddrInDisasmRange addr dstate = unPC (dstate ^. disasmOriginAddr) <= addr && addr <= unPC (dstate ^. disasmEndAddr)
