-- | The Z80 disassembler module
module Z80.Disassembler
  ( -- * Types
    Disassembly
  , DisElement
    -- * Functions
  , z80disassembler 
  ) where

-- import Debug.Trace

import System.IO
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as DS
import Data.Vector.Unboxed (Vector, (!?), (!))
import qualified Data.Vector.Unboxed as DVU
import Data.Bits

import Z80.Processor
import Z80.InstructionSet

type Z80memory = Vector Z80word
type DisElement  = (Z80addr, Vector Z80word, Instruction)
type Disassembly = Seq DisElement

z80disassembler :: Z80memory                    -- ^ Vector of bytes to disassemble
                -> Z80addr                      -- ^ Origin, i.e., output's starting address
                -> Z80addr                      -- ^ Start address, relative to origin, to start disassembling
                -> Z80disp                      -- ^ Number of bytes to disassemble
                -> IO Disassembly
z80disassembler image origin startAddr nBytes 
  | startAddr < origin =
    hPutStrLn stderr (errorStr ++ "start address < origin")
    >> return DS.empty
  | pc + fromIntegral(nBytes) > imgLen =
    hPutStrLn stderr (warnStr ++ "number of bytes to disassemble exceeds image length, truncating")
    >> (return $ disasm image pc (imgLen - pc) DS.empty)
  | otherwise = return $ disasm image pc (pc + fromIntegral(nBytes)) DS.empty
  where
    pc     = startAddr - origin
    imgLen = fromIntegral(DVU.length image) :: Z80addr

disasm :: Z80memory                             -- ^ The image to disassemble
       -> Z80addr                               -- ^ Starting address
       -> Z80addr                               -- ^ Ending address
       -> Disassembly                           -- ^ Disassembled instruction continuation function
       -> Disassembly
disasm image pc lastpc dis
  {- trace ("disasm: pc = " ++ (show pc) ++ ", dis = " ++ (show dis)) False = undefined -}
  | pc >= lastpc =
    dis
  | otherwise =
    let opc = (image !? fromIntegral(pc))
    in  case opc of
          Nothing     -> error ("Z80 disasm: invalid fetch at pc = " ++ (show pc))
          Just theOpc ->
            case theOpc of
              0xcb        -> undefAndNext theOpc
              0xed        -> undefAndNext theOpc
              0xdd        -> undefAndNext theOpc
              0xfd        -> undefAndNext theOpc
              _otherwise ->
                let y  = (shiftR theOpc 3) .&. 7
                    z  = (theOpc .&. 7)
                    p  = (shiftR theOpc 4) .&. 3
                    q  = (shiftR theOpc 3) .&. 1
                in  case (shiftR theOpc 6) .&. 3 of
                      0          -> let (newpc, ins) = group0decode image pc theOpc
                                    in  disasm image (newpc + 1) lastpc (dis |> (pc, getOpcodes pc newpc, ins))
                      1          -> let ins = group1decode theOpc
                                    in  disasm image (pc + 1) lastpc (dis |> (pc, getOpcodes pc pc, ins))
                      2          -> let ins = group2decode theOpc
                                    in  disasm image (pc + 1) lastpc (dis |> (pc, getOpcodes pc pc, ins))
                      3          -> let (newpc, ins) = group3decode image pc theOpc z y p q
                                    in  disasm image (newpc + 1) lastpc (dis |> (pc, getOpcodes pc newpc, ins))
                      _otherwise -> error (errorStr ++ "x out of range?")
  where
    undefAndNext opc = disasm image (pc + 1) lastpc (dis |> (pc, getOpcodes pc pc, Z80undef [opc]))
    getOpcodes x y = let x' = (fromIntegral x) :: Int
                         y' = (fromIntegral y) :: Int
                     in  DVU.slice x' (y' - x' + 1) image

group0decode :: Z80memory                       -- ^ Memory image
             -> Z80addr                         -- ^ Current PC
             -> Z80word                         -- ^ Opcode
             -> (Z80addr, Instruction)          -- ^ (new PC, instruction)

group0decode image pc opc
  | z == 0 = case y of
               0                  -> (pc, NOP)
               1                  -> (pc, EXAFAF')
               2                  -> (pc + 1, DJNZ (getDispAddr image pc))
               3                  -> (pc + 1, JR (getDispAddr image pc))
               _otherwise         -> (pc + 1, JRCC (condC (y - 4)) (getDispAddr image pc))
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
  | z == 6 = (pc, accumOps IntMap.! (fromIntegral y))
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
                   in  aluOp (aluReg8 (opc .&. 7))

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
group3decode :: Z80memory
             -> Z80addr                         -- ^ Current PC
             -> Z80word                         -- ^ Opcode
             -> Z80word                         -- ^ "z" (lower 3 bits)
             -> Z80word                         -- ^ "y" (bits 3-5)
             -> Z80word                         -- ^ "p" (bits 4, 5)
             -> Z80word                         -- ^ "q" (bit 3)
             -> (Z80addr, Instruction)          -- ^ (new PC, instruction)

group3decode _image pc _opc 0 y _p _q = (pc, RETCC (condC y))

group3decode _image pc _opc 1 _y p 0 = (pc, (POP . pairAF) $ p)
group3decode _image pc _opc 1 _y 0 1 = (pc, RET)
group3decode _image pc _opc 1 _y 1 1 = (pc, EXX)
group3decode _image pc _opc 1 _y 2 1 = (pc, JPHL)
group3decode _image pc _opc 1 _y 3 1 = (pc, LDSPHL)

group3decode image pc _opc 2 y _p _q = let cc = condC y
                                           dest = getAddr image (pc + 1)
                                      in   (pc + 2, JPCC cc dest)

group3decode image pc _opc 3 0 _p _q = (pc + 2, JP (getAddr image (pc + 1)))
-- z = 3, y = 1: This is the 0xcb prefix group
group3decode image pc _opc 3 2 _p _q = (pc + 1, OUT (getNextWord image pc))
group3decode image pc _opc 3 3 _p _q = (pc + 1, IN (getNextWord image pc))
group3decode _image pc _opc 3 4 _p _q = (pc, EXSPHL)
group3decode _image pc _opc 3 5 _p _q = (pc, EXDEHL)
group3decode _image pc _opc 3 6 _p _q = (pc, DI)
group3decode _image pc _opc 3 7 _p _q = (pc, EI)

group3decode image pc _opc 4 y _p _q = let cc = condC y
                                           dest = getAddr image (pc + 1)
                                       in  (pc + 2, CALLCC cc dest)

group3decode _image pc _opc 5 _y p 0 = (pc, (PUSH . pairAF $ p))
group3decode image pc _opc 5 _y 0 1 = (pc + 2, CALL (getAddr image (pc + 1)))
-- p = 1, q = 1: DD prefix
-- p = 2, q = 1: ED prefix
-- p = 3, q = 1: FD prefix

group3decode image pc _opc 6 0 _p _q = (pc + 1, ADD (ALUimm (getNextWord image pc)))
group3decode image pc _opc 6 1 _p _q = (pc + 1, ADC (ALUimm (getNextWord image pc)))
group3decode image pc _opc 6 2 _p _q = (pc + 1, SUB (ALUimm (getNextWord image pc)))
group3decode image pc _opc 6 3 _p _q = (pc + 1, SBC (ALUimm (getNextWord image pc)))
group3decode image pc _opc 6 4 _p _q = (pc + 1, AND (ALUimm (getNextWord image pc)))
group3decode image pc _opc 6 5 _p _q = (pc + 1, XOR (ALUimm (getNextWord image pc)))
group3decode image pc _opc 6 6 _p _q = (pc + 1, OR  (ALUimm (getNextWord image pc)))
group3decode image pc _opc 6 7 _p _q = (pc + 1, CP  (ALUimm (getNextWord image pc)))

group3decode _image pc _opc 7 y _p _q = (pc, RST y)

-- Catchall:
group3decode _image pc opc _z _y _p _q = (pc, Z80undef [opc])

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

-- | Fetch an absolute (16-bit) address
getAddr :: Z80memory
        -> Z80addr
        -> Z80addr
getAddr image pc = let pc' = fromIntegral pc :: Int
                       lo = fromIntegral (image ! pc') :: Z80addr
                       hi = fromIntegral (image ! (pc' + 1)) :: Z80addr
                    in (shiftL hi 8) .|. lo

-- | Fetch a displacement, convert it to an address
getDispAddr :: Z80memory
            -> Z80addr
            -> Z80addr
getDispAddr image pc = let pc' = fromIntegral pc :: Int
                           disp = image ! pc'
                       in  pc + (fromIntegral disp)

-- | Get a byte
getWord :: Z80memory
        -> Z80addr
        -> Z80word
getWord image pc = let pc' = fromIntegral pc :: Int
                   in  image ! (pc' + 1)

-- | Get following byte (shorthand)
getNextWord :: Z80memory
            -> Z80addr
            -> Z80word
getNextWord image pc = getWord image (pc + 1)

errorStr :: String
errorStr = "z80disassembler: error: "

warnStr :: String
warnStr = "z80disassembler: warning: "
