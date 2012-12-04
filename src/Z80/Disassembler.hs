{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
--
-- | The Z80 disassembler module
module Z80.Disassembler
  ( -- * Types
    Z80DisasmState(..)
  , Z80DisasmElt(..)
  , Z80PseudoOps(..)
  , Disassembly(..)

    -- * Functions
  , z80disbytes
  , z80disasciiz
  , z80disascii
  , mkInitialDisassembly
  , group0decode
  , group1decode
  , group2decode
  , group3decode
  ) where

-- import Debug.Trace

import Control.Lens
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Vector.Unboxed (Vector, (!?), (!))
import qualified Data.Vector.Unboxed as DVU
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Int
import Data.Bits

import Machine.DisassemblerTypes
import Z80.Processor
import Z80.InstructionSet

-- | Pseudo disassembler operations: These are elements such as bytes to dump, various types of strings, etc.
data Z80PseudoOps where
  -- Disassembly origin marker, generally used in disassembly output
  DisOrigin :: Z80addr
            -> Z80PseudoOps
  -- Range of bytes to dump
  ByteRange :: Z80addr                          -- Start address
            -> Vector Z80word                   -- Bytes
            -> Z80PseudoOps
  -- Byte from an arbitrary expression
  ByteExpression :: Z80addr
                 -> ByteString
                 -> Z80word
                 -> Z80PseudoOps
  -- Address word
  AddrWord  :: Z80addr				-- Start address
            -> OperAddr				-- The address
	    -> Vector Z80word			-- The actual address bytes
	    -> Z80PseudoOps
  -- 0-terminated string (yes, these were used back in the pre-C days...)
  AsciiZ    :: Z80addr                          -- Start of string
            -> Vector Z80word                   -- The string, not including the zero terminator
            -> Z80PseudoOps
  -- Simple ASCII string
  Ascii     :: Z80addr
            -> Vector Z80word
            -> Z80PseudoOps
  -- Address equation: associates a symbol with an address of something, which is also added to the
  -- disassembler's symbol table
  AddrEquate :: ByteString
             -> Z80addr
             -> Z80PseudoOps
  -- Comment, printed as a line, as opposed to after an mnemonic and operands
  LineComment :: ByteString
              -> Z80PseudoOps

-- | Disassembler state, which indexes the 'Disassembly' type family.
data Z80DisasmState =
  Z80DisasmState
  { -- | A general-purpose label counter, useful for local labels, e.g., "L1", "L2", etc., that are inserted
    -- into the symbol table.
    _labelNum :: Int
    -- | Predicate: Is the address in the disassembler's range? Note that the default function always returns 'True'.
  , _addrInDisasmRange :: Z80addr               -- The address to test
                       -> Bool                  -- 'True' if in disassembler's range, 'False' otherwise.
    -- | The symbol table mapping between addresses and symbol names in 'disasmSeq'
  , _symbolTab :: Map Z80addr  ByteString
    -- | The sequence of tuples, each of which is an address, words corresponding to the disassembled instruction, and the
    -- disassembled instruction.
  , _disasmSeq :: Seq Z80DisasmElt
  }

-- | Make an initial 'Z80DisasmState' record
mkInitialDisassembly :: Z80DisasmState
mkInitialDisassembly = Z80DisasmState
                       { _labelNum           = 0
                       , _addrInDisasmRange  = (\_ -> True)
                       , _symbolTab          = Map.empty
                       , _disasmSeq          = Seq.empty
                       }

-- | 'DisasmElement' is a dissassembly element: a disassembled instruction (with corresponding address and instruction
-- words) or pseudo operation.
data Z80DisasmElt where
  DisasmInst   :: Z80addr
               -> Vector Z80word
               -> Z80instruction
               -> Z80DisasmElt
  DisasmPseudo :: Z80PseudoOps
               -> Z80DisasmElt

-- Emit Template Haskell hair for lenses used to set/get/modify the Disassembly state:
makeLenses ''Z80DisasmState
makeLenses ''Z80indexTransform

-- | Where the real work of the Z80 disassembly happens...
disasm :: Z80memory                             -- ^ The "memory" vector of bytes
       -> Z80addr                               -- ^ The disassembly origin
       -> Z80addr                               -- ^ The disassembly start address, realtive to the origin
       -> Z80addr                               -- ^ The disassembly's last address
       -> Disassembly Z80DisasmState
       -> Disassembly Z80DisasmState
disasm theMem origin thePc lastpc (Z80Disassembly dstate)
  {-  | trace ("disasm: pc = " ++ (show thePc)) False = undefined -}
  | thePc >= lastpc = Z80Disassembly dstate
  | otherwise =
    let opc = (theMem !? fromIntegral(thePc))
    in  case opc of
          Nothing     -> error ("Z80 disasm: invalid fetch at pc = " ++ (show thePc))
          Just theOpc -> case theOpc of
                           0xdd       -> indexedPrefix z80ixTransform
                           0xfd       -> indexedPrefix z80iyTransform
                           _otherwise -> let (newpc, ins, dstate')  = decodeInst theMem thePc theOpc dstate z80nullTransform
                                             newDState              = mkDisasmInst thePc newpc ins dstate'
                                         in  disasm theMem origin newpc lastpc (Z80Disassembly newDState)
  where
    mkDisasmInst oldpc newpc ins z80dstate = let disasmPC     = oldpc + origin
                                                 opcodes      = let oldpc' = (fromIntegral oldpc) :: Int
                                                                    newpc' = (fromIntegral newpc) :: Int
                                                                in  DVU.slice oldpc' (newpc' - oldpc') theMem
                                             in  disasmSeq %~ (|> DisasmInst disasmPC opcodes ins) $ z80dstate
    -- Deal with an index register prefix
    indexedPrefix xForms = let newOpc                 = getNextWord theMem thePc
                               (newpc, ins, dstate')  = decodeInst theMem (thePc + 1) newOpc dstate xForms
                               newDState              = mkDisasmInst thePc newpc ins dstate'
                           in  case newOpc of
                                 -- Deal with "weird" IX bit operation layout
                                 0xcb       -> undefined
                                 _otherwise -> disasm theMem origin newpc lastpc (Z80Disassembly newDState)

-- | 'Disassembler' type family instance for the Z80's disassembler
instance Disassembler Z80DisasmState Z80addr Z80word Z80disp where
  -- The disassembly state for the Z80 'Disassembler' instance.
  data Disassembly Z80DisasmState   = Z80Disassembly Z80DisasmState

  disassemble mem origin startAddr nBytes z80dstate =
    let pc = startAddr - origin
        lastpc = pc + (fromIntegral nBytes)
    in  pc `seq` lastpc `seq` disasm mem origin pc lastpc z80dstate

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disbytes :: Z80memory                        -- ^ Vector of bytes from which to extract some data
            -> Z80addr                          -- ^ Output's origin address
            -> Z80addr                          -- ^ Start address, relative to the origin, to start extracting bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Disassembly Z80DisasmState       -- ^ Current disassembly state
            -> Disassembly Z80DisasmState       -- ^ Resulting diassembly state
z80disbytes mem origin sAddr nBytes (Z80Disassembly dstate) =
  let sAddr' = fromIntegral sAddr
      nBytes' = fromIntegral nBytes
      theBytes = ByteRange (sAddr + origin) $ DVU.slice sAddr' nBytes' mem
  in  Z80Disassembly $ disasmSeq %~ (|> DisasmPseudo theBytes) $ dstate

-- | Grab (what is presumably) an ASCII string sequence, terminating at the first 0 encountered. This is somewhat inefficient
-- because multiple 'Vector' slices get created.
z80disasciiz :: Z80memory                       -- ^ Vector of bytes from which to extract some data
             -> Z80addr                         -- ^ Output's origin address
             -> Z80addr                         -- ^ Start address, relative to the origin, to start extracting bytes
             -> Disassembly Z80DisasmState      -- ^ Current disassembly state
             -> Disassembly Z80DisasmState      -- ^ Resulting diassembly state
z80disasciiz mem origin sAddr theDState@(Z80Disassembly dstate) =
  let sAddr'       = fromIntegral sAddr
      toSearch     = DVU.slice sAddr' (DVU.length mem - sAddr') mem
      foundStr idx = DisasmPseudo (AsciiZ (sAddr + origin) (DVU.slice sAddr' (idx + 1) mem))
  in  case DVU.elemIndex 0 toSearch of
        Nothing  -> theDState			-- Not found?
        Just idx -> Z80Disassembly $ disasmSeq %~ (|> foundStr idx) $ dstate

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disascii :: Z80memory                        -- ^ Vector of bytes from which to extract some data
            -> Z80addr                          -- ^ Output's origin address
            -> Z80addr                          -- ^ Start address, relative to the origin, to start extracting bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Disassembly Z80DisasmState       -- ^ Current disassembly state
            -> Disassembly Z80DisasmState       -- ^ Resulting diassembly state
z80disascii mem origin sAddr nBytes (Z80Disassembly dstate) =
  let sAddr'    = fromIntegral sAddr
      nBytes'  = fromIntegral nBytes
      theBytes = Ascii (sAddr + origin) $ DVU.slice sAddr' nBytes' mem
  in  Z80Disassembly $ disasmSeq %~ (|> DisasmPseudo theBytes) $ dstate

-- | Decode one instruction, returning the new program counter and disassembly state.
decodeInst :: Z80memory                         -- ^ The "memory" vector of bytes from which instructions are read
           -> Z80addr                           -- ^ The current PC from which the instruction will be decoded
           -> Z80word                           -- ^ Current opcode at the current PC
           -> Z80DisasmState                    -- ^ Current disassembly state
           -> Z80indexTransform                 -- ^ Register transform functions (convert HL to IX or IY, as needed)
           -> (Z80addr, Z80instruction, Z80DisasmState) -- ^ Resulting disassembly state.
decodeInst theMem pc opc dstate xForms = 
  case (opc `shiftR` 6) .&. 3 of
    0          -> let (newpc, ins, dstate') = group0decode theMem dstate xForms pc opc
                  in  (newpc + 1, ins, dstate')
    1          -> let (newpc, ins) = group1decode xForms theMem pc opc
                  in  (newpc + 1, ins, dstate)
    2          -> let (newpc, ins) = group2decode xForms theMem pc opc
                  in  (newpc + 1, ins, dstate)
    3          -> let (newpc, ins, dstate') = group3decode theMem dstate xForms pc opc
                  in  (newpc + 1, ins, dstate')
    _otherwise -> undefined

group0decode :: Z80memory                       -- ^ Memory image
             -> Z80DisasmState                  -- ^ Current disassembly state
             -> Z80indexTransform               -- ^ Index register transform function
             -> Z80addr                         -- ^ Current PC
             -> Z80word                         -- ^ Opcode
             -> (Z80addr, Z80instruction, Z80DisasmState) -- ^ (new PC, instruction, new disassembly state)

group0decode image dstate xForm pc opc
  | z == 0 = case y of
               0                  -> (pc, NOP, dstate)
               1                  -> (pc, EXAFAF', dstate)
               2                  -> displacementInstruction image dstate pc DJNZ
               3                  -> displacementInstruction image dstate pc JR
               _otherwise         -> displacementInstruction image dstate pc $ JRCC (condC (y - 4))
  | z == 1, q == 0 = symAbsAddress image dstate False "" pc (LD16 (pairSP reg16XFormF p))
  | z == 1, q == 1 = (pc, ADD $ ALU16 (pairSP reg16XFormF p), dstate)
  | z == 2, q == 0 = case p of
                       0          -> (pc, STA BCIndirect, dstate)
                       1          -> (pc, STA DEIndirect, dstate)
                       2          -> symAbsAddress image dstate False "" pc STHL
                       3          -> symAbsAddress image dstate False "" pc (STA . Imm16Indirect)
                       _otherwise -> undefined
  | z == 2, q == 1 = case p of
                       0          -> (pc, LDA BCIndirect, dstate)
                       1          -> (pc, LDA DEIndirect, dstate)
                       2          -> symAbsAddress image dstate False "" pc LDHL
                       3          -> symAbsAddress image dstate False "" pc (LDA . Imm16Indirect)
                       _otherwise -> undefined
  | z == 3 = case q of
               0                  -> (pc, INC16 (pairSP reg16XFormF p), dstate)
               1                  -> (pc, DEC16 (pairSP reg16XFormF p), dstate)
               _otherwise         -> undefined
  | z == 4 = let (newpc, theReg) = reg8 reg8XFormF image pc y
             in  (newpc, INC theReg, dstate)
  | z == 5 = let (newpc, theReg) = reg8 reg8XFormF image pc y
             in  (newpc, DEC theReg, dstate)
  | z == 6 = let (newpc, theReg) = reg8 reg8XFormF image pc y
             in  (newpc + 1, LD8 (Reg8Imm theReg (getNextWord image newpc)), dstate)
  | z == 7 = (pc, accumOps IntMap.! (fromIntegral y), dstate)
  | otherwise = (pc, Z80undef [opc], dstate)
  where
    z           = (opc .&. 7)
    y           = (shiftR opc 3) .&. 7
    p           = (shiftR opc 4) .&. 3
    q           = (shiftR opc 3) .&. 1
    reg8XFormF  = xForm ^. reg8XForm
    reg16XFormF = xForm ^. reg16XForm

-- | Accumulator operations map
accumOps :: IntMap Z80instruction
accumOps = IntMap.fromList [ (0, RLCA)
                           , (1, RRCA)
                           , (2, RLA)
                           , (3, RRA)
                           , (4, DAA)
                           , (5, CPL)
                           , (6, SCF)
                           , (7, CCF)
                           ]

-- | 8-bit loads and HALT
group1decode :: Z80indexTransform
             -> Z80memory
             -> Z80addr
             -> Z80word
             -> (Z80addr, Z80instruction)
group1decode xform mem pc opc
  | z == 6, y == 6  = (pc, HALT)
  | otherwise       = let reg8XFormF      = view reg8XForm xform
                          (pcdst, dstReg) = reg8 reg8XFormF mem pc y
                          (pcsrc, srcReg) = reg8 reg8XFormF mem pc z
                          newpc           = pcdst `max` pcsrc
                      in  (newpc, LD8 (Reg8Reg8 dstReg srcReg))
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7

-- | ALU instruction decode (group 2)
group2decode :: Z80indexTransform               -- ^ HL -> index register transform collection
             -> Z80memory                       -- ^ Z80 memory being disassembled
             -> Z80addr                         -- ^ Current program counter, may be updated by a transform function
             -> Z80word                         -- ^ Opcode
             -> (Z80addr, Z80instruction)       -- ^ Resulting program counter, instruction tuple
group2decode xForms mem pc opc =
  let reg                  = opc .&. 7
      (newPc, alu8operand) = aluReg8 (xForms ^. reg8XForm) mem pc reg
      insCTor    = case opc `shiftR` 3 .&. 7 of 
                     0          -> ADD . ALU8
                     1          -> ADC . ALU8
                     2          -> SUB
                     3          -> SBC . ALU8
                     4          -> AND
                     5          -> XOR
                     6          -> OR
                     7          -> CP
                     _otherwise -> undefined
  in  (newPc, insCTor alu8operand)

-- | Group 3 instructions
group3decode :: Z80memory                       -- ^ Memory image
             -> Z80DisasmState                  -- ^ Current disassembly state
             -> Z80indexTransform               -- ^ HL to IX or IY conversion functions
             -> Z80addr                         -- ^ Current PC
             -> Z80word                         -- ^ Opcode
             -> (Z80addr, Z80instruction, Z80DisasmState) -- ^ (new PC, instruction, new disassembly state)
group3decode image dstate xForm pc opc
  | z == 0          = (pc, RETCC . condC $ y, dstate)
  | z == 1, q == 0  = (pc, (POP . pairAF reg16XFormF) $ p, dstate)
  | z == 1, q == 1  = case p of
                        0          -> (pc, RET, dstate)
                        1          -> (pc, EXX, dstate)
                        2          -> (pc, JPHL, dstate)
                        3          -> (pc, LDSPHL, dstate)
                        _otherwise -> undefined
  | z == 2          = symAbsAddress image dstate True "L" pc (JPCC (condC y))
  | z == 3          = case y of
                        0          -> symAbsAddress image dstate True "L" pc JP
                        -- CB instruction prefix
                        1          -> let nextOpc = getNextWord image pc
                                          ins = bitopsDecode image pc nextOpc
                                      in  (pc + 1, ins, dstate)
                        2          -> (pc + 1, (OUT . PortImm) $ getNextWord image pc, dstate)
                        3          -> (pc + 1, (IN . PortImm)  $ getNextWord image pc, dstate)
                        4          -> (pc, EXSPHL, dstate)
                        5          -> (pc, EXDEHL, dstate)
                        6          -> (pc, DI, dstate)
                        7          -> (pc, EI, dstate)
                        _otherwise -> undefined
  | z == 4           = symAbsAddress image dstate True "SUB" pc (CALLCC (condC y))
  | z == 5, q == 0   = (pc, PUSH $ pairAF reg16XFormF p, dstate)
  | z == 5, q == 1   = case p of
                         0          -> symAbsAddress image dstate True "SUB" pc CALL
                         -- DD instruction prefix (should never reach here.)
                         1          -> undefined
                         2          -> edPrefixDecode image dstate pc
                         3          -> (pc, Z80undef [opc], dstate) -- fd prefix
                         _otherwise -> undefined
  | z == 6           = let imm     = ALUimm (getNextWord image pc)
                           insCTor = case y of
                                       0          -> (ADD . ALU8)
                                       1          -> (ADC . ALU8)
                                       2          -> SUB
                                       3          -> (SBC . ALU8)
                                       4          -> AND
                                       5          -> XOR
                                       6          -> OR
                                       7          -> CP
                                       _otherwise -> undefined
                       in  (pc + 1, insCTor imm, dstate)
  | z == 7           = (pc, RST (y * 8), dstate)
  | otherwise        = (pc, Z80undef [opc], dstate)
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7
    p  = (shiftR opc 4) .&. 3
    q  = (shiftR opc 3) .&. 1
    reg16XFormF = xForm ^. reg16XForm

-- | The SET, RESet, BIT instructions and rotation operations. Note that this is not suitable for dealing with the
-- IX and IY indexed instructions, since the instruction format is 'DDCB <displacement> <opcode>', and has to be
-- handled seperately.
bitopsDecode :: Z80memory                       -- ^ Z80 memory (required to use the null register transform)
             -> Z80addr                         -- ^ Current program counter (required to use the null register transform)
             -> Z80word                         -- ^ Bit/rotate operation opcode
             -> Z80instruction                  -- ^ The result
bitopsDecode mem pc opc =
  case (shiftR opc 6) .&. 3 of
    0          -> let rotOp       = rotOps IntMap.! (fromIntegral y)
                  in  rotOp theReg
    1          -> BIT y theReg
    2          -> RES y theReg
    3          -> SET y theReg
    _otherwise -> undefined
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7
    nullXFormF = z80nullTransform ^. reg8XForm 
    (_, theReg) = reg8 nullXFormF mem pc z

rotOps :: IntMap (Z80reg8 -> Z80instruction)
rotOps = IntMap.fromList [ (0, RLC)
                         , (1, RRC)
                         , (2, RL)
                         , (3, RR)
                         , (4, SLA)
                         , (5, SRA)
                         , (6, SLL)
                         , (7, SRL)
                         ]

-- | Decode 'ED'-prefixed instructions
edPrefixDecode :: Z80memory
               -> Z80DisasmState
               -> Z80addr
               -> (Z80addr, Z80instruction, Z80DisasmState)
edPrefixDecode image dstate pc
  | x == 0 = invalid
  | x == 1, z == 0, y /= 6 = let (_, theReg) = reg8 nullXFormF image pc y
                             in  (pc + 1, IN $ CIndIO theReg , dstate)
  | x == 1, z == 0, y == 6 = (pc + 1, IN CIndIO0, dstate)
  | x == 1, z == 1, y /= 6 = let (_, theReg) = reg8 nullXFormF image pc y
                             in  (pc + 1, OUT $ CIndIO theReg, dstate)
  | x == 1, z == 1, y == 6 = (pc + 1, OUT $ CIndIO0, dstate)
  | x == 1, z == 2, q == 0 = let rp = pairSP nullReg16XFormF p
                             in  (pc + 1, (SBC . ALU16) rp, dstate)
  | x == 1, z == 2, q == 1 = let rp = pairSP nullReg16XFormF p
                             in  (pc + 1, (ADC . ALU16) rp, dstate)
  | x == 1, z == 2         = invalid
  | x == 1, z == 3, q == 0 = let rp    = pairSP nullReg16XFormF p
                                 addr  = (getAddr image (pc + 2))
                             in (pc + 3, ST16Indirect addr rp, dstate)
  | x == 1, z == 3, q == 1 = let rp    = pairSP nullReg16XFormF p
                                 addr  = (getAddr image (pc + 2))
                             in (pc + 3, LD16Indirect rp addr, dstate)
  | x == 1, z == 3         = invalid
  | x == 1, z == 4         = (pc + 1, NEG, dstate)
  | x == 1, z == 5, y == 1 = (pc + 1, RETI, dstate)
  | x == 1, z == 5, y /= 1 = (pc + 1, RETN, dstate)
  | x == 1, z == 6         = (pc + 1, IM (interruptMode IntMap.! (fromIntegral y)), dstate)
  | x == 1, z == 7         = case y of
                               0          -> (pc + 1, (STA IReg), dstate)
                               1          -> (pc + 1, (STA RReg), dstate)
                               2          -> (pc + 1, (LDA IReg), dstate)
                               3          -> (pc + 1, (LDA RReg), dstate)
                               4          -> (pc + 1, RLD, dstate)
                               5          -> (pc + 1, RRD, dstate)
                               6          -> invalid
                               7          -> invalid
                               _otherwise -> error ("edPrefixDecode, x = 1, z = 7: invalid y = " ++ (show y))
  | x == 2, z <= 3, y >= 4 = -- Increment, Increment-Repeat instructions
                             (pc + 1, ((incdecOps IntMap.! (fromIntegral y)) IntMap.! (fromIntegral z)), dstate)
  | x == 2                 = invalid
  | x == 3                 = invalid
  -- Should never be matched...
  | otherwise              = error ("edPrefixDecode: could not decode instruction, x == "
                                    ++ (show x)
                                    ++ ", z == "
                                    ++ (show z)
                                    ++ ", y == "
                                    ++ (show y))
  where
    opc = getNextWord image pc
    x = (opc `shiftR` 6) .&. 3
    y = (opc `shiftR` 3) .&. 7
    z = (opc .&. 7)
    p = (y `shiftR` 1) .&. 3
    q = y .&. 1
    invalid = (pc + 1, Z80undef [0xed, opc], dstate)
    nullXFormF = z80nullTransform ^. reg8XForm 
    nullReg16XFormF = z80nullTransform ^. reg16XForm 

-- | Block/compare/input/output increment-decrement lookup table
incdecOps :: IntMap (IntMap Z80instruction)
incdecOps = IntMap.fromList [ (4, IntMap.fromList [ (0, LDI )
                                                  , (1, CPI )
                                                  , (2, INI )
                                                  , (3, OUTI )
                                                  ] )
                            , (5, IntMap.fromList [ (0, LDD)
                                                  , (1, CPD)
                                                  , (2, IND)
                                                  , (3, OUTD)
                                                  ] )
                            , (6, IntMap.fromList [ (0, LDIR)
                                                  , (1, CPIR)
                                                  , (2, INIR)
                                                  , (3, OTIR)
                                                  ] )
                            , (7, IntMap.fromList [ (0, LDDR)
                                                  , (1, CPDR)
                                                  , (2, INDR)
                                                  , (3, OTDR)
                                                  ] )
                            ]

-- | Convert embedded interrupt mode to actual interrupt mode 
interruptMode :: IntMap Z80word
interruptMode = IntMap.fromList [ (0, 0)
                                , (1, 0)        -- Could be either 0 or 1, actually
                                , (2, 1)
                                , (3, 2)
                                , (4, 0)
                                , (5, 0)        -- Could be either 0 or 1, actually
                                , (6, 1)
                                , (7, 2)
                                ]

-- | Convert 16-bit register pair/SP index to a 'RegPairSP' operand
pairSP :: (Z80reg16 -> Z80reg16)                -- ^ Transform function for index registers, when neeeded
       -> Z80word                               -- ^ Register code
       -> RegPairSP                             -- ^ Resulting register pair
pairSP _xform 0 = RPair16 BC
pairSP _xform 1 = RPair16 DE
pairSP  xform 2 = (RPair16 . xform) HL
pairSP _xform 3 = SP
pairSP _      x = error ("pairSP: invalid 16-bit register code " ++ (show x))

-- | Convert 16-bit register pair/SP index to a 'RegPairSP' operand
pairAF :: (Z80reg16 -> Z80reg16)                -- ^ Transform function, when needed
       -> Z80word                               -- ^ Register pair index
       -> RegPairAF                             -- ^ Resulting register pair
pairAF _xform 0 = RPair16' BC
pairAF _xform 1 = RPair16' DE
pairAF  xform 2 = (RPair16' . xform) HL
pairAF _xform 3 = AF
pairAF _      x = error ("pairAF: invalid 16-bit register code " ++ (show x))

-- | Convert 8-bit register index to a 'Z80reg8' operand
reg8 :: Z80reg8XForm
     -> Z80memory
     -> Z80addr
     -> Z80word
     -> (Z80addr, Z80reg8)
reg8 _xform _mem  pc 0 = (pc, B)
reg8 _xform _mem  pc 1 = (pc, C)
reg8 _xform _mem  pc 2 = (pc, D)
reg8 _xform _mem  pc 3 = (pc, E)
reg8 _xform _mem  pc 4 = (pc, H)
reg8 _xform _mem  pc 5 = (pc, L)
reg8  xform  mem  pc 6 = xform mem pc HLindirect
reg8 _xform _mem  pc 7 = (pc, A)
reg8 _xform _mem _pc  x = error ("reg8: Invalid 8-bit register code " ++ (show x))

-- | Convert an 8-bit register index to an ALU operand 'OperALU'
aluReg8 :: Z80reg8XForm
        -> Z80memory
        -> Z80addr
        -> Z80word
        -> (Z80addr, OperALU)
aluReg8 xform mem pc operand = let (newpc, theReg) = (reg8 xform mem pc operand)
                               in  (newpc, ALUreg8 theReg)

-- | Convert condition code
condC :: Z80word
      -> Z80condC
condC 0 = NZ
condC 1 = Z
condC 2 = NC
condC 3 = CY
condC 4 = PO
condC 5 = PE
condC 6 = POS
condC 7 = MI
condC x = error ("condC: Invalid condition code index " ++ (show x))

-- | Compute signed, relative displacement address' absolute address, generating a label for it
-- if not already in the disassembler's symbol table.
displacementInstruction :: Z80memory
                        -> Z80DisasmState
                        -> Z80addr
                        -> (OperAddr -> Z80instruction)
                        -> (Z80addr, Z80instruction, Z80DisasmState)
displacementInstruction mem dstate pc ins =
  let pc'   = fromIntegral (pc + 1) :: Int
      disp  = (fromIntegral (mem ! pc'):: Int16)
      disp' = if disp <= 0x7f then
                disp
              else
                -((disp `xor` 0xff) + 1)
      symTab       = view symbolTab dstate
      addrInRangeF = view addrInDisasmRange dstate
      destAddr     = pc + (fromIntegral disp') + 2
      isInSymtab   = Map.member destAddr symTab
  in  if (addrInRangeF destAddr) && not isInSymtab then
        let label   = BC.cons 'L' (BC.pack . show $ view labelNum dstate)
        in (pc + 1, (ins . SymAddr) label, (symbolTab %~ (Map.insert destAddr label)) . (labelNum +~ 1) $ dstate)
      else if isInSymtab then
             (pc + 1, (ins . SymAddr) $ symTab Map.! destAddr, dstate)
           else
             (pc + 1, (ins . AbsAddr) destAddr, dstate)

-- | Generate a label for an absolute address, if in the disassembler's range and no other label
-- exists for the address.
symAbsAddress :: Z80memory                      -- ^ Z80 "memory"
              -> Z80DisasmState                 -- ^ Current disassembly state
              -> Bool                           -- ^ 'True': generate a label for the address, if one doesn't already exist
              -> ByteString                     -- ^ Label prefix, if one is generated
              -> Z80addr
              -> (OperAddr -> Z80instruction)
              -> (Z80addr, Z80instruction, Z80DisasmState)
symAbsAddress image dstate makeLabel prefix pc ins =
  let destAddr     = getAddr image (pc + 1)
      symTab       = view symbolTab dstate
      addrInRangeF = view addrInDisasmRange dstate
      isInSymtab   = Map.member destAddr symTab
      defaultTuple = (pc + 2, (ins . AbsAddr) destAddr, dstate)
  in  if (addrInRangeF destAddr) && not isInSymtab then
        if makeLabel then
          let label   = BC.append prefix (BC.pack . show $ view labelNum dstate)
          in (pc + 2, (ins . SymAddr) label, (symbolTab %~ (Map.insert destAddr label)) . (labelNum +~ 1) $ dstate)
        else
          defaultTuple
      else if isInSymtab then
             (pc + 2, (ins . SymAddr) $ symTab Map.! destAddr, dstate)
           else
             defaultTuple

-- | Fetch an absolute (16-bit) address
getAddr :: Z80memory
        -> Z80addr
        -> Z80addr
getAddr image pc = let pc' = fromIntegral pc :: Int
                       lo = fromIntegral (image ! pc') :: Z80addr
                       hi = fromIntegral (image ! (pc' + 1)) :: Z80addr
                    in (shiftL hi 8) .|. lo
