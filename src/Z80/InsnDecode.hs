module Z80.InsnDecode 
  ( -- * Types
    Z80decodedInsn

    -- * Functions
  , z80insnDecode
  , z80getAddr
  ) where

import Control.Lens
import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Machine
import Z80.Processor
import Z80.InstructionSet

-- | 'DecodedInsn' shorthand for Z80 decoded instructions
type Z80decodedInsn = DecodedInsn Z80instruction Z80addr

-- | Decode one instruction, returning the new program counter and disassembly state.
z80insnDecode :: Z80PC                  -- ^ Current program counter
              -> Z80memory memSys       -- ^ The memory system from which bytes are fetched
              -> Z80decodedInsn         -- ^ The decoded instruction
z80insnDecode pc mem =
  let opc = getOpcode pc mem
      indexedPrefix xForms = let pc'    = pcInc pc
                                 newOpc = getOpcode pc' mem
                             in  case newOpc of
                                   -- Deal with "weird" IX/IY bit operation layout
                                   0xcb       -> error "Z80 IX/IY bit instructions not decoded yet."
                                   _otherwise -> decodeF newOpc mem pc' xForms
      -- N.B. The opcode 'x' is passed through to the decoding function
      decodeF x = case (x `shiftR` 6) .&. 3 of
                    0          -> group0decode x
                    1          -> group1decode x
                    2          -> group2decode x
                    3          -> group3decode x
                    _otherwise -> undefined
  in  case opc of
          0xdd       -> indexedPrefix z80ixTransform
          0xfd       -> indexedPrefix z80iyTransform
          _otherwise -> decodeF opc mem pc z80nullTransform

-- | Instruction group 0 decoder: 
group0decode :: Z80word                         -- ^ Current opcode
             -> Z80memory memSys                -- ^ Memory image
             -> Z80PC                           -- ^ Current program counter
             -> Z80indexTransform memSys        -- ^ Index register transform function
             -> Z80decodedInsn                  -- ^ (Instruction, New disassembly state)

group0decode opc mem pc xForm
  | z == 0 = case y of
               0                  -> defResult NOP
               1                  -> defResult (EXC AFAF')
               2                  -> displacementInstruction mem pc DJNZ
               3                  -> displacementInstruction mem pc JR
               _otherwise         -> displacementInstruction mem pc $ JRCC (condC (y - 4))
  | z == 1, q == 0 = mkAbsAddrIns (LD . (RPair16ImmLoad (pairSP reg16XFormF p))) mem pc
  | z == 1, q == 1 = defResult ((ADD . ALU16) $ pairSP reg16XFormF p)
  | z == 2, q == 0 = case p of
                       0          -> defResult (LD BCIndirectStore)
                       1          -> defResult (LD DEIndirectStore)
                       2          -> mkAbsAddrIns (LD . HLIndirectStore) mem pc
                       3          -> mkAbsAddrIns (LD . Imm16IndirectStore) mem pc
                       _otherwise -> undefined
  | z == 2, q == 1 = case p of
                       0          -> defResult (LD AccBCIndirect)
                       1          -> defResult (LD AccDEIndirect)
                       2          -> mkAbsAddrIns (LD . HLIndirectLoad) mem pc
                       3          -> mkAbsAddrIns (LD . AccImm16Indirect) mem pc
                       _otherwise -> undefined
  | z == 3 = case q of
               0                  -> defResult (INC16 (pairSP reg16XFormF p))
               1                  -> defResult (DEC16 (pairSP reg16XFormF p))
               _otherwise         -> undefined
  | z == 4 = let (newpc, theReg) = reg8 reg8XFormF mem pc y
             in  DecodedInsn (pcInc newpc) (INC theReg)
  | z == 5 = let (newpc, theReg) = reg8 reg8XFormF mem pc y
             in  DecodedInsn (pcInc newpc) (DEC theReg)
  | z == 6 = let (newpc, theReg)   = reg8 reg8XFormF mem pc y
                 newpc'            = pcInc newpc
                 immval            = getOpcode newpc' mem
             in  DecodedInsn (pcInc newpc') (LD (Reg8Imm theReg immval))
  | z == 7 = defResult (accumOps IntMap.! (fromIntegral y))
  | otherwise = defResult (Z80undef [opc])
  where
    z             = (opc .&. 7)
    y             = (shiftR opc 3) .&. 7
    p             = (shiftR opc 4) .&. 3
    q             = (shiftR opc 3) .&. 1
    reg8XFormF    = xForm ^. reg8XForm
    reg16XFormF   = xForm ^. reg16XForm
    nextIns       = pcInc pc
    defResult ins = DecodedInsn nextIns ins

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
             -> Z80indexTransform memSys
             -> Z80decodedInsn
group1decode opc mem pc xform
  | z == 6, y == 6  = DecodedInsn (pcInc pc) HALT
  | otherwise       = let reg8XFormF         = xform ^. reg8XForm
                          (newpc, dstReg)  = reg8 reg8XFormF mem pc y
                          (newpc', srcReg) = reg8 reg8XFormF mem newpc z
                      in  DecodedInsn (pcInc newpc') (LD (Reg8Reg8 dstReg srcReg))
  where
    z = (opc .&. 7)
    y = (shiftR opc 3) .&. 7

-- | ALU instruction decode (group 2)
group2decode :: Z80word
             -> Z80memory memSys                        -- ^ Z80 memory being disassembled
             -> Z80PC                                   -- ^ Current program counter
             -> Z80indexTransform memSys                -- ^ HL -> index register transform collection
             -> Z80decodedInsn                          -- ^ Decoded result
group2decode opc mem pc xForms =
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
  in  DecodedInsn (pcInc newpc) (insCTor alu8operand)

-- | Group 3 instructions
group3decode :: Z80word
             -> Z80memory memSys                -- ^ System memory being disassembled
             -> Z80PC                           -- ^ Program counter
             -> Z80indexTransform memSys        -- ^ HL to IX or IY conversion functions
             -> Z80decodedInsn                  -- ^ Decoded result
group3decode opc mem pc xForm
  | z == 0          = defResult (RETCC . condC $ y)
  | z == 1, q == 0  = defResult ((POP . pairAF reg16XFormF) $ p)
  | z == 1, q == 1  = case p of
                        0          -> defResult RET
                        1          -> defResult (EXC Primes)
                        2          -> defResult JPHL
                        3          -> defResult LDSPHL
                        _otherwise -> undefined
  | z == 2          = mkAbsAddrIns (JPCC (condC y)) mem pc
  | z == 3          = case y of
                        0          -> mkAbsAddrIns JP mem pc
                        -- CB instruction prefix
                        1          -> let newpc   = pcInc pc
                                          nextOpc = getOpcode newpc mem
                                      in  DecodedInsn (pcInc newpc) (bitopsDecode mem newpc nextOpc)
                        2          -> let newpc   = pcInc pc
                                          nextOpc = getOpcode newpc mem
                                      in  DecodedInsn (pcInc newpc) ((OUT . PortImm) $ nextOpc)
                        3          -> let newpc = pcInc pc
                                          nextOpc = getOpcode newpc mem
                                      in  DecodedInsn (pcInc newpc) ((IN . PortImm)  $ nextOpc)
                        4          -> defResult (EXC SPHL)
                        5          -> defResult (EXC DEHL)
                        6          -> defResult DI
                        7          -> defResult EI
                        _otherwise -> undefined
  | z == 4           = mkAbsAddrIns (CALLCC (condC y)) mem pc
  | z == 5, q == 0   = defResult (PUSH $ pairAF reg16XFormF p)
  | z == 5, q == 1   = case p of
                         0          -> mkAbsAddrIns CALL mem pc
                         -- DD instruction prefix (should never reach here.)
                         1          -> undefined
                         -- ED instruction prefix
                         2          -> let newpc   = pcInc pc
                                           nextOpc = getOpcode newpc mem
                                      in  edPrefixDecode nextOpc mem newpc
                         -- FD instruction prefix (should never reach here.)
                         3          -> undefined
                         _otherwise -> undefined
  | z == 6           = let newpc   = pcInc pc
                           nextOpc = getOpcode newpc mem
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
                       in  DecodedInsn (pcInc newpc) (insCTor imm)
  | z == 7           = defResult (RST (y * 8))
  | otherwise        = defResult (Z80undef [opc])
  where
    z             = (opc .&. 7)
    y             = (shiftR opc 3) .&. 7
    p             = (shiftR opc 4) .&. 3
    q             = (shiftR opc 3) .&. 1
    reg16XFormF   = xForm ^. reg16XForm
    nextIns       = pcInc pc
    defResult ins = DecodedInsn nextIns ins

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
               -> Z80decodedInsn
edPrefixDecode opc mem pc
  | x == 0 = invalid
  | x == 1, z == 0, y /= 6 = let (newpc, reg) = reg8 nullXFormF mem pc y
                             in  DecodedInsn (pcInc newpc) (IN . CIndIO $ reg)
  | x == 1, z == 0, y == 6 = defResult (IN CIndIO0)
  | x == 1, z == 1, y /= 6 = let (newpc, reg) = reg8 nullXFormF mem (pcInc newpc) y
                             in  DecodedInsn (pcInc newpc) ((OUT . CIndIO) reg)
  | x == 1, z == 1, y == 6 = defResult (OUT CIndIO0)
  | x == 1, z == 2, q == 0 = defResult ((SBC . ALU16) $ pairSP nullReg16XFormF p)
  | x == 1, z == 2, q == 1 = defResult ((ADC . ALU16) $ pairSP nullReg16XFormF p)
  | x == 1, z == 2         = invalid
  | x == 1, z == 3, q == 0 = mkAbsAddrIns (LD . (RPIndirectStore (pairSP nullReg16XFormF p))) mem (pcInc pc)
  | x == 1, z == 3, q == 1 = mkAbsAddrIns (LD . (RPIndirectLoad (pairSP nullReg16XFormF p))) mem (pcInc pc) 
  | x == 1, z == 3         = invalid
  | x == 1, z == 4         = defResult NEG
  | x == 1, z == 5, y == 1 = defResult RETI
  | x == 1, z == 5, y /= 1 = defResult RETN
  | x == 1, z == 6         = defResult (IM (interruptMode IntMap.! (fromIntegral y)))
  | x == 1, z == 7         = case y of
                               0          -> defResult (LD IRegAcc)
                               1          -> defResult (LD RRegAcc)
                               2          -> defResult (LD AccIReg)
                               3          -> defResult (LD AccRReg)
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
    defResult ins   = DecodedInsn nextIns ins 

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
                                , (1, 0)        -- Could be either 0 or 1, not clear from instruction set description
                                , (2, 1)
                                , (3, 2)
                                , (4, 0)
                                , (5, 0)        -- Could be either 0 or 1, not clear from instruction set description
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
                        -> ((SymAbsAddr Z80addr) -> Z80instruction)
                        -> Z80decodedInsn
displacementInstruction mem pc ins =
  let pc'     = pcInc pc
      disp    = getOpcode pc' mem
      disp'        = signExtend disp
      destAddr     = fromIntegral (((fromIntegral . getPCvalue) pc) + disp' + 2)
      nextIns      = pcInc pc'
  in  DecodedInsn nextIns (ins . AbsAddr $ destAddr)

-- | Get opcode at current program counter
getOpcode :: Z80PC
          -> Z80memory memSys
          -> Z80word
getOpcode pc mem =  mem ^. mfetch $ (getPCvalue pc)

-- | Fetch address, insert into an instruction
mkAbsAddrIns :: (SymAbsAddr Z80addr -> Z80instruction)
             -> Z80memory memSys
             -> Z80PC
             -> Z80decodedInsn
mkAbsAddrIns ins mem pc =
  let DecodedAddr pc' addr = z80getAddr mem (pcInc pc)
  in  DecodedInsn pc' ((ins . AbsAddr) addr)

-- | Fetch an absolute (16-bit) little endian address
z80getAddr :: Z80memory memSys          -- ^ Memory from which address is fetched
           -> Z80PC                     -- ^ The program counter
           -> Z80decodedInsn
z80getAddr mem pc = let lo         = fromIntegral ((mem ^. mfetch) (getPCvalue pc))
                        pc'        = pcInc pc
                        hi         = fromIntegral ((mem ^. mfetch) (getPCvalue pc'))
                    in DecodedAddr (pcInc pc') ((shiftL hi 8) .|. lo)
