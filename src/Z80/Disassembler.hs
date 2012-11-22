{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- | The Z80 disassembler module
module Z80.Disassembler
  ( -- * Types
    Z80memory
  , Z80Disassembly
  , Z80PseudoOps(..)

    -- * Functions
  , z80disassembler
  , z80disbytes
  , z80disasciiz
  , z80disascii
  , group0decode
  , group1decode
  , group2decode
  , group3decode
  ) where

-- import Debug.Trace

import Data.Label
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence ((|>))
import Data.Vector.Unboxed (Vector, (!?), (!))
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Int
import Data.Bits

import Machine.DisassemblerTypes
import Z80.Processor
import Z80.InstructionSet

-- Emit Template Haskell hair for lenses used to set/get/modify the Disassembly state:
mkLabels [ ''Disassembly ]

-- | Pseudo disassembler operations: These are elements such as bytes to dump, various types of strings, etc.
data Z80PseudoOps where
  -- Range of bytes to dump
  ByteRange :: Z80addr                          -- Start address
            -> Vector Z80word                   -- Bytes
            -> Z80PseudoOps
  -- 0-terminated string (yes, these were used back in the pre-C days...)
  AsciiZ    :: Z80addr                          -- Start of string
            -> Vector Z80word                   -- The string, not including the zero terminator
            -> Z80PseudoOps
  -- Simple ASCII string
  Ascii     :: Z80addr
            -> Vector Z80word
            -> Z80PseudoOps
  -- Symbol equation (associates a symbol with a number, usually the address of something
  Equate :: Z80addr
         -> ByteString
         -> Z80PseudoOps

-- | Shorthand for the 'Vector' of Z80words
type Z80memory = Vector Z80word
-- | Shorthand for the 'Disassembly' state for the Z80
type Z80Disassembly = Disassembly Z80addr Z80word Instruction Z80PseudoOps

-- | The Z80 disassembler, which just invokes the 'Disassembly' class instance of 'disassemble'
z80disassembler :: Z80memory                    -- ^ Vector of bytes to disassemble
                -> Z80addr                      -- ^ Origin, i.e., output's starting address
                -> Z80addr                      -- ^ Start address, relative to origin, to start disassembling
                -> Z80disp                      -- ^ Number of bytes to disassemble
                -> Z80Disassembly               -- ^ Existing list of disassembled instructions
                -> Z80Disassembly
z80disassembler image origin startAddr nBytes disassembled = disassemble image origin startAddr nBytes disassembled

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disbytes :: Z80memory                        -- ^ Vector of bytes from which to extract some data
            -> Z80addr                          -- ^ Output's origin address
            -> Z80addr                          -- ^ Start address, relative to the origin, to start extracting bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Z80Disassembly                   -- ^ Current disassembly state
            -> Z80Disassembly                   -- ^ Resulting diassembly state
z80disbytes mem origin sAddr nBytes dstate = let sAddr' = fromIntegral sAddr
                                                 nBytes' = fromIntegral nBytes
                                                 theBytes = ByteRange (sAddr + origin) $ DVU.slice sAddr' nBytes' mem
                                                 disSeq   = get disasmSeq dstate
                                             in  set disasmSeq (disSeq |> DisasmPseudo theBytes) dstate

-- | Grab (what is presumably) an ASCII string sequence, terminating at the first 0 encountered. This is somewhat inefficient
-- because multiple 'Vector' slices get created.
z80disasciiz :: Z80memory                       -- ^ Vector of bytes from which to extract some data
             -> Z80addr                         -- ^ Output's origin address
             -> Z80addr                         -- ^ Start address, relative to the origin, to start extracting bytes
             -> Z80Disassembly                  -- ^ Current disassembly state
             -> Z80Disassembly                  -- ^ Resulting diassembly state
z80disasciiz mem origin sAddr dstate = let sAddr'       = fromIntegral sAddr
                                           toSearch     = DVU.slice sAddr' (DVU.length mem - sAddr') mem
                                           disSeq       = get disasmSeq dstate
                                           foundStr idx = DisasmPseudo (AsciiZ (sAddr + origin) (DVU.slice sAddr' (idx + 1) mem))
                                in  case DVU.elemIndex 0 toSearch of
                                      Nothing -> dstate -- No found?
                                      Just idx -> set disasmSeq (disSeq |> foundStr idx) dstate

-- | Grab a sequence of bytes from the memory image, add to the disassembly sequence as a 'ByteRange' pseudo instruction
z80disascii :: Z80memory                        -- ^ Vector of bytes from which to extract some data
            -> Z80addr                          -- ^ Output's origin address
            -> Z80addr                          -- ^ Start address, relative to the origin, to start extracting bytes
            -> Z80disp                          -- ^ Number of bytes to extract
            -> Z80Disassembly                   -- ^ Current disassembly state
            -> Z80Disassembly                   -- ^ Resulting diassembly state
z80disascii mem origin sAddr nBytes dstate = let sAddr'    = fromIntegral sAddr
                                                 nBytes'  = fromIntegral nBytes
                                                 theBytes = Ascii (sAddr + origin) $ DVU.slice sAddr' nBytes' mem
                                                 disSeq   = get disasmSeq dstate
                                             in  set disasmSeq (disSeq |> DisasmPseudo theBytes) dstate

-- | The 'Disassembly' class instance for the Z80.
instance Disassembler Z80addr Z80disp Z80word Instruction Z80PseudoOps where
    disassemble mem origin startAddr nBytes disassembled = disasm mem origin pc lastpc disassembled
      where
        pc = startAddr - origin
        lastpc = pc + (fromIntegral nBytes)

-- | Where the real work of the Z80 disassembly happens...
disasm :: Z80memory                             -- ^ The "memory" vector of bytes
       -> Z80addr                               -- ^ The disassembly origin
       -> Z80addr                               -- ^ The disassembly start address, realtive to the origin
       -> Z80addr                               -- ^ The disassembly's last address
       -> Z80Disassembly
       -> Z80Disassembly
disasm theMem origin thePc lastpc dis
  {-  | trace ("disasm: pc = " ++ (show thePc)) False = undefined -}
  | thePc >= lastpc = dis
  | otherwise =
    let opc = (theMem !? fromIntegral(thePc))
    in  case opc of
          Nothing     -> error ("Z80 disasm: invalid fetch at pc = " ++ (show thePc))
          Just theOpc -> case (shiftR theOpc 6) .&. 3 of
                           0          -> let (newpc, ins) = group0decode theMem thePc theOpc
                                         in  disasm theMem origin (newpc + 1) lastpc $ mkDisasmInst thePc newpc ins
                           1          -> let ins = group1decode theOpc
                                         in  disasm theMem origin (thePc + 1) lastpc $ mkDisasmInst thePc thePc ins
                           2          -> let ins = group2decode theOpc
                                         in  disasm theMem origin (thePc + 1) lastpc $ mkDisasmInst thePc thePc ins
                           3          -> let (newpc, ins) = group3decode theMem thePc theOpc
                                         in  disasm theMem origin (newpc + 1) lastpc $ mkDisasmInst thePc newpc ins
                           _otherwise -> error (errorStr ++ "x out of range?")
  where
    -- Save the opcode bytes, now that we know how many correspond the the instruction
    getOpcodes x y = let x' = (fromIntegral x) :: Int
                         y' = (fromIntegral y) :: Int
                     in  DVU.slice x' (y' - x' + 1) theMem
    -- Note: 'set' and 'get' come from Data.Label.
    mkDisasmInst oldpc newpc ins = let disasmPC = oldpc + origin
                                       opcodes = getOpcodes oldpc newpc
                                       newDisasmSeq = (get disasmSeq dis) |> DisasmInst disasmPC opcodes ins
                                   in  set disasmSeq newDisasmSeq $ annotateAddr ins dis
    -- Annotate labels on instructions that contain addresses
    annotateAddr ins theDState = case ins of
                                   (DJNZ addr)     -> jumpLabel addr theDState
                                   (JR addr)       -> jumpLabel addr theDState
                                   (JRCC _cc addr) -> jumpLabel addr theDState
                                   (CALL addr)     -> subLabel  addr theDState
                                   _otherwise      -> theDState
    -- Create a label signifying the destination of a jump
    jumpLabel addr theDState = let symTab = get symbolTab theDState
                              in  if Map.notMember addr symTab then
                                    let label = BC.append "L" (BC.pack . show $ get labelNum theDState)
                                        updsyms = Map.insert addr label symTab
                                    in  set symbolTab updsyms $ modify labelNum (+ 1) theDState
                                  else
                                    theDState   -- No change to the disassembler state
  -- Create a label signifying the destination of a subrouting
    subLabel addr theDState = let symTab = get symbolTab theDState
                              in  if Map.notMember addr symTab then
                                    theDState
                                  else
                                    theDState

group0decode :: Z80memory                       -- ^ Memory image
             -> Z80addr                         -- ^ Current PC
             -> Z80word                         -- ^ Opcode
             -> (Z80addr, Instruction)          -- ^ (new PC, instruction)

group0decode image pc opc
  | z == 0 = case y of
               0                  -> (pc, NOP)
               1                  -> (pc, EXAFAF')
               2                  -> displacementInstruction image pc DJNZ
               3                  -> displacementInstruction image pc JR
               _otherwise         -> displacementInstruction image pc $ JRCC (condC (y - 4))
  | z == 1 = case q of
               0                  -> (pc + 2, LD16 (pairSP p) (getAddr image (pc + 1)))
               1                  -> (pc, ADDHL (pairSP p))
               _otherwise         -> undefined
  | z == 2, q == 0 = case p of
                       0          -> (pc, STA BCIndirect)
                       1          -> (pc, STA DEIndirect)
                       2          -> (pc + 2, STHL (getAddr image (pc + 1)))
                       3          -> (pc + 2, STA (Imm16Indirect (getAddr image (pc + 1))))
                       _otherwise -> undefined
  | z == 2, q == 1 = case p of
                       0          -> (pc, LDA BCIndirect)
                       1          -> (pc, LDA DEIndirect)
                       2          -> (pc + 2, LDHL (getAddr image (pc + 1)))
                       3          -> (pc + 2, LDA (Imm16Indirect (getAddr image (pc + 1))))
                       _otherwise -> undefined
  | z == 3 = case q of
               0                  -> (pc, INC16 (pairSP p))
               1                  -> (pc, DEC16 (pairSP p))
               _otherwise         -> undefined
  | z == 4 = (pc, INC (reg8 y))
  | z == 5 = (pc, DEC (reg8 y))
  | z == 6 = (pc + 1, LD8 (Reg8Imm (reg8 y) (getNextWord image pc)))
  | z == 7 = (pc, accumOps IntMap.! (fromIntegral y))
  | otherwise = (pc, Z80undef [opc])
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7
    p  = (shiftR opc 4) .&. 3
    q  = (shiftR opc 3) .&. 1

-- | Accumulator operations map
accumOps :: IntMap Instruction
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
group1decode :: Z80word
             -> Instruction
group1decode opc 
  | z == 6, y == 6  = HALT
  | otherwise       = LD8 (Reg8Reg8 (reg8 y) (reg8 z))
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7

-- | ALU instruction decode (group 2)
group2decode :: Z80word
             -> Instruction
group2decode opc = let aluOp = aluOps IntMap.! (fromIntegral $ (shiftR opc 3) .&. 7)
                   in  (aluOp . aluReg8) $ (opc .&. 7)

-- | ALU operation map (reduces pattern matching)
aluOps :: IntMap (OperALU -> Instruction)
aluOps = IntMap.fromList [ (0, ADD)
                         , (1, ADC)
                         , (2, SUB)
                         , (3, SBC)
                         , (4, AND)
                         , (5, XOR)
                         , (6, OR)
                         , (7, CP)
                         ]

-- | Group 3 instructions
group3decode :: Z80memory                       -- ^ Memory image
             -> Z80addr                         -- ^ Current PC
             -> Z80word                         -- ^ Opcode
             -> (Z80addr, Instruction)          -- ^ (new PC, instruction)
group3decode image pc opc
  | z == 0          = (pc, RETCC . condC $ y)
  | z == 1, q == 0  = (pc, (POP . pairAF) $ p)
  | z == 1, q == 1  = case p of
                        0          -> (pc, RET)
                        1          -> (pc, EXX)
                        2          -> (pc, JPHL)
                        3          -> (pc, LDSPHL)
                        _otherwise -> undefined
  | z == 2          = (pc + 2, JPCC (condC y) (getAddr image (pc + 1)))
  | z == 3          = case y of
                        0          -> (pc + 2, JP (getAddr image (pc + 1)))
                        1          -> bitopsDecode image pc
                        2          -> (pc + 1, OUT (getNextWord image pc))
                        3          -> (pc + 1, IN (getNextWord image pc))
                        4          -> (pc, EXSPHL)
                        5          -> (pc, EXDEHL)
                        6          -> (pc, DI)
                        7          -> (pc, EI)
                        _otherwise -> undefined
  | z == 4           = (pc + 2, CALLCC (condC y) (getAddr image (pc + 1)))
  | z == 5, q == 0   = (pc, PUSH . pairAF $ p)
  | z == 5, q == 1   = case p of
                         0          -> (pc + 2, CALL (getAddr image (pc + 1)))
                         1          -> (pc, Z80undef [opc]) -- dd prefix
                         2          -> edPrefixDecode image pc
                         3          -> (pc, Z80undef [opc]) -- fd prefix
                         _otherwise -> undefined
  | z == 6           = let aluOp = aluOps IntMap.! (fromIntegral y)
                       in  (pc + 1, aluOp (ALUimm (getNextWord image pc)))
  | z == 7           = (pc, RST y)
  | otherwise        = (pc, Z80undef [opc])
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7
    p  = (shiftR opc 4) .&. 3
    q  = (shiftR opc 3) .&. 1

bitopsDecode :: Z80memory                       -- ^ Memory
             -> Z80addr                         -- ^ Current PC (but we increment it locally)
             -> (Z80addr, Instruction)          -- ^ The result
bitopsDecode image pc = case (shiftR opc 6) .&. 3 of
                           0          -> let rotOp = rotOps IntMap.! (fromIntegral y)
                                         in  (pc + 1, (rotOp . reg8) $ z)
                           1          -> (pc + 1, (BIT y (reg8 z)))
                           2          -> (pc + 1, (RES y (reg8 z)))
                           3          -> (pc + 1, (SET y (reg8 z)))
                           _otherwise -> undefined
  where
    opc = getNextWord image pc
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7

rotOps :: IntMap (Z80reg8 -> Instruction)
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
               -> Z80addr
               -> (Z80addr, Instruction)
edPrefixDecode image pc = case (shiftR opc 6) .&. 3 of
                            0         -> invalid
                            1         -> case z of
                                           0 -> invalid
                                           1 -> invalid
                                           2 -> let rp = pairSP p
                                                in  (pc + 1, if q == 0 then
                                                               SBCHL rp
                                                             else
                                                               ADCHL rp)
                                           3 -> let rp    = pairSP p
                                                    addr  = (getAddr image (pc + 1))
                                                in  if q == 0 then
                                                  (pc + 2, ST16Indirect addr rp)
                                                else
                                                  (pc + 2, LD16Indirect rp addr)
                                           4 -> (pc + 1, NEG)
                                           5 -> (pc + 1, if y == 1 then
                                                           RETI
                                                         else
                                                           RETN)
                                                  
                                           6 -> (pc + 1, IM (interruptMode IntMap.! (fromIntegral y)))
                                           7 -> case y of
                                                  0 -> (pc + 1, (STA IReg))
                                                  1 -> (pc + 1, (STA RReg))
                                                  2 -> (pc + 1, (LDA IReg))
                                                  3 -> (pc + 1, (LDA RReg))
                                                  4 -> (pc + 1, RLD)
                                                  5 -> (pc + 1, RRD)
                                                  6 -> invalid
                                                  7 -> invalid
                            2         -> if z <= 3 && y >= 4 then
                                           -- Increment, Increment-Repeat instructions
                                           (pc + 1, ((incdecOps IntMap.! (fromIntegral y)) IntMap.! (fromIntegral z)))
                                         else
                                           invalid
                            3         -> invalid
                            _otherise -> invalid
  where
    opc = getNextWord image pc
    z = (opc .&. 7)
    y = (opc `shiftR` 3) .&. 7
    p = (y `shiftR` 1) .&. 3
    q = y .&. 1
    invalid = (pc + 1, Z80undef [0xed, opc])

-- | Block/compare/input/output increment-decrement lookup table
incdecOps :: IntMap (IntMap Instruction)
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
pairSP :: Z80word
       -> RegPairSP
pairSP 0 = RPair16 BC
pairSP 1 = RPair16 DE
pairSP 2 = RPair16 HL
pairSP 3 = SP
pairSP x = error ("pairSP: invalid 16-bit register code " ++ (show x))

-- | Convert 16-bit register pair/SP index to a 'RegPairSP' operand
pairAF :: Z80word
       -> RegPairAF
pairAF 0 = RPair16' BC
pairAF 1 = RPair16' DE
pairAF 2 = RPair16' HL
pairAF 3 = AF
pairAF x = error ("pairAF: invalid 16-bit register code " ++ (show x))

-- | Convert 8-bit register index to a 'Z80reg8' operand
reg8 :: Z80word
     -> Z80reg8
reg8 0 = B
reg8 1 = C
reg8 2 = D
reg8 3 = E
reg8 4 = H
reg8 5 = L
reg8 6 = HLindirect
reg8 7 = A
reg8 x = error ("reg8: Invalid 8-bit register code " ++ (show x))

-- | Convert an 8-bit register index to an ALU operand 'OperALU'
aluReg8 :: Z80word
        -> OperALU
aluReg8 = ALUreg8 . reg8

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

-- | Fetch an absolute (16-bit) address
getAddr :: Z80memory
        -> Z80addr
        -> Z80addr
getAddr image pc = let pc' = fromIntegral pc :: Int
                       lo = fromIntegral (image ! pc') :: Z80addr
                       hi = fromIntegral (image ! (pc' + 1)) :: Z80addr
                    in (shiftL hi 8) .|. lo

displacementInstruction :: Z80memory
                        -> Z80addr
                        -> (Z80addr -> Instruction)
                        -> (Z80addr, Instruction)
displacementInstruction image pc ins = let pc'   = fromIntegral (pc + 1) :: Int
                                           disp  = fromIntegral (image ! pc') :: Int16
                                           disp' = fromIntegral (if disp <= 0x7f then
                                                                   disp
                                                                 else
                                                                   -((disp `xor` 0xff) + 1))
                                       in  (pc + 1, ins $ (pc + disp' + 2))

-- | Get a byte
getWord :: Z80memory
        -> Z80addr
        -> Z80word
getWord image pc = let pc' = fromIntegral pc :: Int
                   in  image ! pc'

-- | Get following byte (shorthand)
getNextWord :: Z80memory
            -> Z80addr
            -> Z80word
getNextWord image pc = getWord image (pc + 1)

errorStr :: String
errorStr = "z80disassembler: error: "
