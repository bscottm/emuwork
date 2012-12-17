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
  , group0decode
  , group1decode
  , group2decode
  , group3decode
  , z80DisGetAddr
  , z80DefaultPostProcessor

  -- * Lens functions for 'Z80disassembly'
  , labelNum
  , addrInDisasmRange
  , symbolTab
  , disasmSeq
  ) where

-- import Debug.Trace

import Control.Lens
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as DVU
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import Data.Bits

import Machine
import Z80.Processor
import Z80.InstructionSet

-- | Disassembly elements for the Z80
type Z80DisasmElt = DisElement Z80instruction Z80addr Z80word Z80PseudoOps

-- | Pseudo disassembler operations: These are elements such as bytes to dump, various types of strings, etc.
data Z80PseudoOps where
  -- Byte from an arbitrary expression
  ByteExpression :: Z80addr
                 -> ByteString
                 -> Word8
                 -> Z80PseudoOps

-- | Z80 instruction or pseudo operation contains an address?
isZ80AddrIns :: Z80DisasmElt
             -> Bool
isZ80AddrIns (ExtPseudo (ByteExpression _ _ _)) = True
isZ80AddrIns op                                 = disEltHasAddr op

-- | Extract address component from a Z80 disassembler element
z80InsAddr :: Z80DisasmElt
           -> Z80addr
z80InsAddr (ExtPseudo (ByteExpression addr _ _)) = addr
z80InsAddr op                                    = disEltGetAddr op

-- | Get the instruction or pseudo operation's length
z80InsLength :: Z80DisasmElt
             -> Int
z80InsLength (ExtPseudo (ByteExpression _ _ _)) = 1
z80InsLength op                                 = disEltGetLength op

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
  , _symbolTab :: Map Z80addr  ByteString
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
                       , _symbolTab          = Map.empty
                       , _disasmSeq          = Seq.empty
                       }

-- | 'decodeInst' and related functions\' return type.
data DecodeResult = Decoded Z80instruction Z80PC Z80disassembly

-- | Where the real work of the Z80 disassembly happens...
disasm :: Z80disassembly                        -- ^ Incoming disassembly sequence and state
       -> Z80memory memSys                      -- ^ System memory
       -> Z80PC                                 -- ^ Current program counter
       -> Z80PC                                 -- ^ The disassembly's last address
       -> ( Z80DisasmElt
            -> MemorySystem Z80addr Z80word memSys
            -> Z80PC
            -> Z80disassembly
            -> (Z80PC, Z80disassembly)
          )                                     -- ^ Post-processing function
       -> Z80disassembly                        -- ^ Resulting disassmbly sequence and state
disasm dstate theMem thePC lastpc postProc
  {-  | trace tracemsg False = undefined -}
  | thePC <= lastpc =
    let opc = theMem ^. mfetch $ (getPCvalue thePC)
    in  case opc of
          0xdd       -> indexedPrefix z80ixTransform
          0xfd       -> indexedPrefix z80iyTransform
          _otherwise -> let Decoded ins newpc newdstate = decodeInst opc theMem thePC dstate z80nullTransform
                            disasmElt                   = mkDisasmInst thePC newpc ins
                            (newpc', newdstate')        = postProc disasmElt theMem newpc newdstate
                        in  disasm newdstate' theMem newpc' lastpc postProc
  | otherwise = dstate
  where
    mkDisasmInst (PC oldpc) (PC newpc) ins =
      let opcodes      = let oldpc' = (fromIntegral oldpc) :: Int
                             newpc' = (fromIntegral newpc) :: Int
                         in  (theMem ^. mfetchN) oldpc (newpc' - oldpc')
      in  DisasmInsn oldpc opcodes ins ""
    -- Deal with an index register prefix
    indexedPrefix xForms = let pc'                         = pcInc thePC
                               newOpc                      = (theMem ^. mfetch) $ getPCvalue pc'
                               Decoded ins newpc newdstate = decodeInst newOpc theMem pc' dstate xForms
                               disasmElt                   = mkDisasmInst thePC newpc ins
                               (newpc', newdstate')        = postProc disasmElt theMem newpc newdstate
                           in  case newOpc of
                                 -- Deal with "weird" IX bit operation layout
                                 0xcb       -> undefined
                                 _otherwise -> disasm  newdstate' theMem newpc' lastpc postProc
    -- tracemsg = "disasm: " ++ (show thePC) ++ ", lastpc = " ++ (show lastpc) ++ ", thePC <= lastpc: " ++ (show $ thePC <= lastpc)

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disbytes :: Z80disassembly                   -- ^ Current disassembly state
            -> Z80memory memSys                 -- ^ Vector of bytes from which to extract some data
            -> Z80PC                            -- ^ Start address from which to grab bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Z80disassembly                   -- ^ Resulting diassembly state
z80disbytes dstate mem (PC sAddr) nBytes =
  disasmSeq %~ (|> (ByteRange sAddr $ (mem ^. mfetchN) sAddr (fromIntegral nBytes))) $ dstate

-- | Grab (what is presumably) an ASCII string sequence, terminating at the first 0 encountered. This is somewhat inefficient
-- because multiple 'Vector' slices get created.
z80disasciiz :: Z80disassembly                  -- ^ Current disassembly state
             -> Z80memory memSys                -- ^ Vector of bytes from which to extract some data
             -> Z80PC                           -- ^ Start address
             -> Z80disassembly                  -- ^ Resulting diassembly state
z80disasciiz dstate mem (PC sAddr) =
  let sRange       = (mem ^. maxmem) - sAddr
      toSearch     = (mem ^. mfetchN) sAddr (fromIntegral sRange)
      foundStr idx = AsciiZ sAddr (DVU.slice 0 (idx + 1) toSearch)
  in  case DVU.elemIndex 0 toSearch of
        Nothing  -> dstate                      -- Not found?
        Just idx -> disasmSeq %~ (|> foundStr idx) $ dstate

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disascii :: Z80disassembly                   -- ^ Current disassembly state
            -> Z80memory memSys                 -- ^ Vector of bytes from which to extract some data
            -> Z80PC                            -- ^ Start address from which to start extracting bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Z80disassembly                   -- ^ Resulting diassembly state
z80disascii dstate mem (PC sAddr) nBytes =
  disasmSeq %~ (|> (Ascii sAddr $ (mem ^. mfetchN) sAddr (fromIntegral nBytes))) $ dstate

-- | Decode one instruction, returning the new program counter and disassembly state.
decodeInst :: Z80word                           -- ^ Opcode to decode
           -> Z80memory memSys                  -- ^ The "memory" vector of bytes from which instructions are read
           -> Z80PC                             -- ^ Current program counter
           -> Z80disassembly                    -- ^ Z80 disassembly state
           -> Z80indexTransform memSys          -- ^ Register transform functions (convert HL to IX or IY, as needed)
           -> DecodeResult                      -- ^ Resulting disassembly state.

decodeInst opc mem pc dstate xForms = decodeF opc mem pc dstate xForms
  where
    decodeF = case (opc `shiftR` 6) .&. 3 of
                0          -> group0decode
                1          -> group1decode
                2          -> group2decode
                3          -> group3decode
                _otherwise -> undefined

-- | Instruction group 0 decoder: 
group0decode :: Z80word                         -- ^ Current opcode
             -> Z80memory memSys                -- ^ Memory image
             -> Z80PC                           -- ^ Current program counter
             -> Z80disassembly                  -- ^ Disassembly state
             -> Z80indexTransform memSys        -- ^ Index register transform function
             -> DecodeResult                    -- ^ (Instruction, New disassembly state)

group0decode opc mem pc dstate xForm
  | z == 0 = case y of
               0                  -> defResult NOP
               1                  -> defResult (EXC AFAF')
               2                  -> displacementInstruction mem pc dstate DJNZ
               3                  -> displacementInstruction mem pc dstate JR
               _otherwise         -> displacementInstruction mem pc dstate $ JRCC (condC (y - 4))
  | z == 1, q == 0 = symAbsAddress mem nextIns dstate False "" (LD16 (pairSP reg16XFormF p))
  | z == 1, q == 1 = defResult ((ADD . ALU16) $ pairSP reg16XFormF p)
  | z == 2, q == 0 = case p of
                       0          -> defResult (STA BCIndirect)
                       1          -> defResult (STA DEIndirect)
                       2          -> symAbsAddress mem nextIns dstate False "" STHL
                       3          -> symAbsAddress mem nextIns dstate False "" (STA . Imm16Indirect)
                       _otherwise -> undefined
  | z == 2, q == 1 = case p of
                       0          -> defResult (LDA BCIndirect)
                       1          -> defResult (LDA DEIndirect)
                       2          -> symAbsAddress mem nextIns dstate False "" LDHL
                       3          -> symAbsAddress mem nextIns dstate False "" (LDA . Imm16Indirect)
                       _otherwise -> undefined
  | z == 3 = case q of
               0                  -> defResult (INC16 (pairSP reg16XFormF p))
               1                  -> defResult (DEC16 (pairSP reg16XFormF p))
               _otherwise         -> undefined
  | z == 4 = let (newpc, theReg) = reg8 reg8XFormF mem pc y
             in  Decoded (INC theReg) (pcInc newpc) dstate
  | z == 5 = let (newpc, theReg) = reg8 reg8XFormF mem pc y
             in  Decoded (DEC theReg) (pcInc newpc) dstate
  | z == 6 = let (newpc, theReg)   = reg8 reg8XFormF mem pc y
                 newpc'            = pcInc newpc
                 immval            = (mem ^. mfetch) (getPCvalue newpc')
             in  Decoded (LD8 (Reg8Imm theReg immval)) (pcInc newpc') dstate
  | z == 7 = defResult (accumOps IntMap.! (fromIntegral y))
  | otherwise = defResult (Z80undef [opc])
  where
    nextIns       = pcInc pc
    z             = (opc .&. 7)
    y             = (shiftR opc 3) .&. 7
    p             = (shiftR opc 4) .&. 3
    q             = (shiftR opc 3) .&. 1
    reg8XFormF    = xForm ^. reg8XForm
    reg16XFormF   = xForm ^. reg16XForm
    defResult ins = Decoded ins nextIns dstate

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

-- | Instruction group 1: 8-bit loads and HALT
group1decode :: Z80word
             -> Z80memory memSys          
             -> Z80PC
             -> Z80disassembly
             -> Z80indexTransform memSys
             -> DecodeResult
group1decode opc mem pc dstate xform
  | z == 6, y == 6  = Decoded HALT (pcInc pc) dstate
  | otherwise       = let reg8XFormF         = xform ^. reg8XForm
                          (newpc, dstReg)  = reg8 reg8XFormF mem pc y
                          (newpc', srcReg) = reg8 reg8XFormF mem newpc z
                      in  Decoded (LD8 (Reg8Reg8 dstReg srcReg)) (pcInc newpc') dstate
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7

-- | ALU instruction decode (group 2)
group2decode :: Z80word
             -> Z80memory memSys                        -- ^ Z80 memory being disassembled
             -> Z80PC                                   -- ^ Current program counter
             -> Z80disassembly                          -- ^ Disassembly state
             -> Z80indexTransform memSys                -- ^ HL -> index register transform collection
             -> DecodeResult                            -- ^ Decoded result
group2decode opc mem pc dstate xForms =
  let (newpc, alu8operand) = aluReg8 (xForms ^. reg8XForm) mem pc (opc .&. 7)
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
  in  Decoded (insCTor alu8operand) (pcInc newpc) dstate

-- | Group 3 instructions
group3decode :: Z80word
             -> Z80memory memSys                -- ^ System memory being disassembled
             -> Z80PC                           -- ^ Program counter
             -> Z80disassembly                  -- ^ Current disassembly state
             -> Z80indexTransform memSys        -- ^ HL to IX or IY conversion functions
             -> DecodeResult                    -- ^ Decoded result
group3decode opc mem pc dstate xForm
  | z == 0          = defResult (RETCC . condC $ y)
  | z == 1, q == 0  = defResult ((POP . pairAF reg16XFormF) $ p)
  | z == 1, q == 1  = case p of
                        0          -> defResult RET
                        1          -> defResult (EXC Primes)
                        2          -> defResult JPHL
                        3          -> defResult LDSPHL
                        _otherwise -> undefined
  | z == 2          = symAbsAddress mem nextIns dstate True "L" (JPCC (condC y))
  | z == 3          = case y of
                        0          -> symAbsAddress mem nextIns dstate True "L" JP
                        -- CB instruction prefix
                        1          -> let newpc   = pcInc pc
                                          nextOpc = (mem ^. mfetch) $ (getPCvalue newpc)
                                      in  Decoded (bitopsDecode mem newpc nextOpc) (pcInc newpc) dstate
                        2          -> let newpc   = pcInc pc
                                          nextOpc = (mem ^. mfetch) $ (getPCvalue newpc)
                                      in  Decoded ((OUT . PortImm) $ nextOpc) (pcInc newpc) dstate
                        3          -> let newpc = pcInc pc
                                          nextOpc = (mem ^. mfetch) $ (getPCvalue newpc)
                                      in  Decoded ((IN . PortImm)  $ nextOpc) (pcInc newpc) dstate
                        4          -> defResult (EXC SPHL)
                        5          -> defResult (EXC DEHL)
                        6          -> defResult DI
                        7          -> defResult EI
                        _otherwise -> undefined
  | z == 4           = symAbsAddress mem nextIns dstate True "SUB" (CALLCC (condC y))
  | z == 5, q == 0   = defResult (PUSH $ pairAF reg16XFormF p)
  | z == 5, q == 1   = case p of
                         0          -> symAbsAddress mem nextIns dstate True "SUB" CALL
                         -- DD instruction prefix (should never reach here.)
                         1          -> undefined
                         -- ED instruction prefix
                         2          -> let newpc   = pcInc pc
                                           nextOpc = (mem ^. mfetch) $ (getPCvalue newpc)
                                      in  edPrefixDecode nextOpc mem newpc dstate
                         -- FD instruction prefix (should never reach here.)
                         3          -> undefined
                         _otherwise -> undefined
  | z == 6           = let newpc   = pcInc pc
                           nextOpc = (mem ^. mfetch) $ (getPCvalue newpc)
                           imm     = ALUimm nextOpc
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
                       in  Decoded (insCTor imm) (pcInc newpc) dstate
  | z == 7           = defResult (RST (y * 8))
  | otherwise        = defResult (Z80undef [opc])
  where
    nextIns       = pcInc pc
    z             = (opc .&. 7)
    y             = (shiftR opc 3) .&. 7
    p             = (shiftR opc 4) .&. 3
    q             = (shiftR opc 3) .&. 1
    reg16XFormF   = xForm ^. reg16XForm
    defResult ins = Decoded ins nextIns dstate

-- | The SET, RESet, BIT instructions and rotation operations. Note that this is not suitable for dealing with the
-- IX and IY indexed instructions, since the instruction format is 'DDCB <displacement> <opcode>', and has to be
-- handled seperately.
bitopsDecode :: Z80memory memSys                -- ^ Z80 memory (required to use the null register transform)
             -> Z80PC                           -- ^ Disassembly state, queried for current program counter
             -> Z80word                         -- ^ Bit/rotate operation opcode
             -> Z80instruction                  -- ^ The result
bitopsDecode mem pc opc =
  case (shiftR opc 6) .&. 3 of
    0          -> (rotOps IntMap.! (fromIntegral y)) theReg
    1          -> BIT y theReg
    2          -> RES y theReg
    3          -> SET y theReg
    _otherwise -> undefined
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7
    -- Ignore any new program counter 'reg8' because we always apply the null transform
    (_, theReg) = reg8 (z80nullTransform ^. reg8XForm) mem pc z

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
edPrefixDecode :: Z80word
               -> Z80memory memSys          
               -> Z80PC
               -> Z80disassembly
               -> DecodeResult
edPrefixDecode opc mem pc dstate
  | x == 0 = invalid
  | x == 1, z == 0, y /= 6 = let (newpc, reg) = reg8 nullXFormF mem pc y
                             in  Decoded (IN . CIndIO $ reg) (pcInc newpc) dstate
  | x == 1, z == 0, y == 6 = defResult (IN CIndIO0)
  | x == 1, z == 1, y /= 6 = let (newpc, reg) = reg8 nullXFormF mem (pcInc newpc) y
                             in  Decoded ((OUT . CIndIO) reg) (pcInc newpc) dstate
  | x == 1, z == 1, y == 6 = defResult (OUT CIndIO0)
  | x == 1, z == 2, q == 0 = defResult ((SBC . ALU16) $ pairSP nullReg16XFormF p)
  | x == 1, z == 2, q == 1 = defResult ((ADC . ALU16) $ pairSP nullReg16XFormF p)
  | x == 1, z == 2         = invalid
  | x == 1, z == 3, q == 0 = let rp               = pairSP nullReg16XFormF p
                                 newpc            = pcInc pc
                                 (newpc', addr)   = z80DisGetAddr mem newpc
                             in Decoded (ST16Indirect addr rp) newpc' dstate
  | x == 1, z == 3, q == 1 = let rp               = pairSP nullReg16XFormF p
                                 newpc            = pcInc pc
                                 (newpc', addr)   = z80DisGetAddr mem newpc
                             in Decoded (LD16Indirect rp addr) newpc' dstate
  | x == 1, z == 3         = invalid
  | x == 1, z == 4         = defResult NEG
  | x == 1, z == 5, y == 1 = defResult RETI
  | x == 1, z == 5, y /= 1 = defResult RETN
  | x == 1, z == 6         = defResult (IM (interruptMode IntMap.! (fromIntegral y)))
  | x == 1, z == 7         = case y of
                               0          -> defResult (STA IReg)
                               1          -> defResult (STA RReg)
                               2          -> defResult (LDA IReg)
                               3          -> defResult (LDA RReg)
                               4          -> defResult RLD
                               5          -> defResult RRD
                               6          -> invalid
                               7          -> invalid
                               _otherwise -> error ("edPrefixDecode, x = 1, z = 7: invalid y = " ++ (show y))
  | x == 2, z <= 3, y >= 4 = -- Increment, Increment-Repeat instructions
                             defResult $ (incdecOps IntMap.! (fromIntegral y)) IntMap.! (fromIntegral z)
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
    x               = (opc `shiftR` 6) .&. 3
    y               = (opc `shiftR` 3) .&. 7
    z               = (opc .&. 7)
    p               = (y `shiftR` 1) .&. 3
    q               = y .&. 1
    invalid         = defResult (Z80undef [0xed, opc])
    nullXFormF      = z80nullTransform ^. reg8XForm
    nullReg16XFormF = z80nullTransform ^. reg16XForm
    nextIns         = pcInc pc
    defResult ins   = Decoded ins nextIns dstate

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
reg8 :: Z80reg8XForm memSys                     -- ^ Register transform function, used for IX/IY transformations
     -> Z80memory memSys                        -- ^ Memory from which to fetch IX/IY displacements
     -> Z80PC                                   -- ^ Disassembler state
     -> Z80word                                 -- ^ 8-bit register index
     -> (Z80PC, Z80reg8)                        -- ^ Register and new disassembly state
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
--
-- This uses Control.Lens to apply the 'ALUreg8' data constructor on the first element of the pair
-- returned by 'reg8'

aluReg8 :: Z80reg8XForm memSys                          -- ^ IX/IY transform function, when needed
        -> Z80memory memSys                             -- ^ Memory from which IX/IY displacements are fetched
        -> Z80PC                                        -- ^ Current program counter
        -> Z80word                                      -- ^ Register code
        -> (Z80PC, OperALU)

aluReg8 xform mem pc operand = _2 %~ ALUreg8 $ reg8 xform mem pc operand

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
displacementInstruction :: Z80memory memSys          
                        -> Z80PC
                        -> Z80disassembly
                        -> ((SymAbsAddr Z80addr) -> Z80instruction)
                        -> DecodeResult
displacementInstruction mem pc dstate ins =
  let pc'     = pcInc pc
      disp    = (mem ^. mfetch) (getPCvalue pc')
      disp'   = if disp <= 0x7f then
                  disp
                else
                  -((disp `xor` 0xff) + 1)
      symTab       = dstate ^. symbolTab
      addrInRangeF = dstate ^. addrInDisasmRange
      destAddr     = (getPCvalue pc) + (fromIntegral disp') + 2
      isInSymtab   = Map.member destAddr symTab
      nextIns      = pcInc pc'
  in  if (addrInRangeF destAddr) && not isInSymtab then
        let label   = BC.cons 'L' (BC.pack . show $ dstate ^. labelNum)
        in Decoded (ins . SymAddr $ label) nextIns ((symbolTab %~ (Map.insert destAddr label)) . (labelNum +~ 1) $ dstate)
      else if isInSymtab then
             Decoded ((ins . SymAddr) $ symTab Map.! destAddr) nextIns dstate
           else
             Decoded (ins . AbsAddr $ destAddr) nextIns dstate

-- | Generate a label for an absolute address, if in the disassembler's range and no other label
-- exists for the address.
symAbsAddress :: Z80memory memSys               -- ^ Z80 "memory"
              -> Z80PC                          -- ^ Program counter, from which the address will be fetched
              -> Z80disassembly                 -- ^ Current disassembly state
              -> Bool                           -- ^ 'True': generate a label for the address, if one doesn't already exist
              -> ByteString                     -- ^ Label prefix, if one is generated
              -> ((SymAbsAddr Z80addr) -> Z80instruction)
              -> DecodeResult
symAbsAddress mem pc dstate makeLabel prefix ins =
  let (addrpc, destAddr) = z80DisGetAddr mem pc
      symTab             = dstate ^. symbolTab
      addrInRangeF       = dstate ^. addrInDisasmRange
      isInSymtab         = Map.member destAddr symTab
      defaultTuple       = Decoded (ins . AbsAddr $ destAddr) addrpc dstate
  in  if (addrInRangeF destAddr) && not isInSymtab then
        if makeLabel then
          let label   = BC.append prefix (BC.pack . show $ dstate ^. labelNum)
          in  Decoded ((ins . SymAddr) label) addrpc ((symbolTab %~ (Map.insert destAddr label)) . (labelNum +~ 1) $ dstate)
        else
          defaultTuple
      else if isInSymtab then
             Decoded ((ins . SymAddr) $ symTab Map.! destAddr) addrpc dstate
           else
             defaultTuple

-- | Fetch an absolute (16-bit) address
z80DisGetAddr :: Z80memory memSys               -- ^ Memory from which address is fetched
              -> Z80PC                          -- ^ The program counter
              -> (Z80PC, Z80addr)               -- ^ Address
z80DisGetAddr mem pc = let pcAddr     = (getPCvalue pc)
                           lo         = fromIntegral ((mem ^. mfetch) pcAddr)
                           newpc      = pcInc pc
                           newpcAddr  = getPCvalue newpc
                           hi         = fromIntegral ((mem ^. mfetch) newpcAddr)
                       in (pcInc newpc, (shiftL hi 8) .|. lo)

-- | 'Disassembler' type family instance for the Z80's disassembler
instance Disassembler Z80disassembly Z80instruction Z80addr Z80word Z80PseudoOps where
  disassemble = disasm

-- | Z80 default instruction post processor. This merely appends the decoded instruction onto the disassembly sequence.
-- types cannot be deduced correctly.
z80DefaultPostProcessor :: Z80DisasmElt
                        -> Z80memory memsys
                        -> Z80PC
                        -> Z80disassembly
                        -> (Z80PC, Z80disassembly)
z80DefaultPostProcessor elt _mem pc z80dstate = (pc, disasmSeq %~ (|> elt) $ z80dstate)
