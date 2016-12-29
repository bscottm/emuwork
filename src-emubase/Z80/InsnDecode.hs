{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Z80.InsnDecode
  ( -- * Types
    Z80decodedInsn

    -- * Functions
  , z80InsDecode
  , z80getAddr
  ) where

import           Control.Lens
import           Data.Bits
import           Data.IntMap        (IntMap, (!))
import qualified Data.IntMap        as IntMap

-- import Debug.Trace

import           Machine
import           Z80.InstructionSet
import           Z80.Processor
import           Z80.System

-- | 'DecodedInsn' shorthand for decoded Z80 instructions and resulting memory system
type Z80decodedInsn sysType = (DecodedInsn Z80instruction Z80addr, Z80system sysType)

instance ProcessorOps Z80state Z80instruction Z80addr Z80word where
  idecode = z80InsDecode

z80InsDecode :: ProgramCounter Z80addr
             -> Z80system sysType
             -> Z80decodedInsn sysType
z80InsDecode pc z80sys =
  let (opc, z80sys')             = mRead (unPC pc) z80sys
      indexedPrefix xForm idxSys =
        case mIncPCAndRead pc idxSys of
          (pc', (0xcb, sys'))   -> undocBitOpsDecode opc pc' sys'
          (pc', (newOpc, sys')) -> decodeF newOpc sys' pc' xForm
      -- N.B. The opcode 'x' is passed through to the decoding function
      decodeF x = case (x `shiftR` 6) .&. 3 of
                    0          -> group0decode x
                    1          -> group1decode x
                    2          -> group2decode x
                    3          -> group3decode x
                    _otherwise -> undefined
  in  case opc of
        0xdd       -> indexedPrefix z80ixTransform z80sys'
        0xfd       -> indexedPrefix z80iyTransform z80sys'
        _otherwise -> decodeF opc z80sys' pc z80nullTransform

-- | Instruction group 0 decoder:
group0decode :: Z80word
             -- ^ Current opcode
             -> Z80system sysType
             -- ^ Memory image
             -> Z80PC
             -- ^ Current program counter
             -> Z80indexTransform sysType
             -- ^ Index register transform function
             -> Z80decodedInsn sysType
             -- ^ (Instruction, New disassembly state)

group0decode opc sys pc xForm
  | z == 0 = case y of
               0          -> defResult NOP
               1          -> defResult (EXC AFAF')
               2          -> displacementInstruction sys pc DJNZ
               3          -> displacementInstruction sys pc JR
               _otherwise -> displacementInstruction sys pc $ JRCC (condC (y - 4))
  | z == 1, q == 0 = mkAbsAddrIns (LD . RPair16ImmLoad (pairSP reg16XFormF p)) sys pc
  | z == 1, q == 1 = defResult ((ADD . ALU16) $ pairSP reg16XFormF p)
  | z == 2, q == 0 = case p of
                       0          -> defResult (LD BCIndirectStore)
                       1          -> defResult (LD DEIndirectStore)
                       2          -> mkAbsAddrIns (LD . HLIndirectStore) sys pc
                       3          -> mkAbsAddrIns (LD . Imm16IndirectStore) sys pc
                       _otherwise -> undefined
  | z == 2, q == 1 = case p of
                       0          -> defResult (LD AccBCIndirect)
                       1          -> defResult (LD AccDEIndirect)
                       2          -> mkAbsAddrIns (LD . HLIndirectLoad) sys pc
                       3          -> mkAbsAddrIns (LD . AccImm16Indirect) sys pc
                       _otherwise -> undefined
  | z == 3 = case q of
               0          -> defResult (INC16 (pairSP reg16XFormF p))
               1          -> defResult (DEC16 (pairSP reg16XFormF p))
               _otherwise -> undefined
  | z == 4 = let (newpc, theReg, sys')     = reg8 reg8XFormF sys pc y
             in  (DecodedInsn (newpc + 1) (INC theReg), sys')
  | z == 5 = let (newpc, theReg, sys')     = reg8 reg8XFormF sys pc y
             in  (DecodedInsn (newpc + 1) (DEC theReg), sys')
  | z == 6 = let (newpc, theReg, sys')     = reg8 reg8XFormF sys pc y
                 (newpc', (immval, sys'')) = mIncPCAndRead newpc sys'
             in  (DecodedInsn (newpc' + 1) (LD (Reg8Imm theReg immval)), sys'')
  | z == 7 = defResult (accumOps ! fromIntegral y)
  | otherwise = defResult (Z80undef [opc])
  where
    z             = opc .&. 7
    y             = shiftR opc 3 .&. 7
    p             = shiftR opc 4 .&. 3
    q             = shiftR opc 3 .&. 1
    reg8XFormF    = reg8XForm xForm
    reg16XFormF   = reg16XForm xForm
    defResult ins = (DecodedInsn (pc + 1) ins, sys)

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
             -> Z80system sysType
             -> Z80PC
             -> Z80indexTransform sysType
             -> Z80decodedInsn sysType
group1decode opc sys pc xform
  | z == 6, y == 6  = (DecodedInsn (pc + 1) HALT, sys)
  | otherwise       = let reg8XFormF              = reg8XForm xform
                          (newpc, dstReg, sys')   = reg8 reg8XFormF sys pc y
                          (newpc', srcReg, sys'') = reg8 reg8XFormF sys' newpc z
                      in  (DecodedInsn (newpc' + 1) (LD (Reg8Reg8 dstReg srcReg)), sys'')
  where
    z = opc .&. 7
    y = shiftR opc 3 .&. 7

-- | ALU instruction decode (group 2)
group2decode :: Z80word
             -> Z80system sysType
             -- ^ Z80 memory being disassembled
             -> Z80PC
             -- ^ Current program counter
             -> Z80indexTransform sysType
             -- ^ HL -> index register transform collection
             -> Z80decodedInsn sysType
             -- ^ Decoded result
group2decode opc sys pc xForms =
  let (newpc, alu8operand, sys') = aluReg8 (reg8XForm xForms) sys pc (opc .&. 7)
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
  in  (DecodedInsn (newpc + 1) (insCTor alu8operand), sys')

-- | Group 3 instructions
group3decode :: Z80word
             -> Z80system sysType
             -- ^ System memory being disassembled
             -> Z80PC
             -- ^ Program counter
             -> Z80indexTransform sysType
             -- ^ HL to IX or IY conversion functions
             -> Z80decodedInsn sysType
             -- ^ Decoded result
group3decode opc sys pc xForm
  | z == 0          = defResult (RETCC . condC $ y)
  | z == 1          = case q of
                        0 -> defResult ((POP . pairAF reg16XFormF) p)
                        1 -> case p of
                               0          -> defResult RET
                               1          -> defResult (EXC Primes)
                               2          -> defResult JPHL
                               3          -> defResult LDSPHL
                               _otherwise -> undefined
                        _otherwise -> undefined
  | z == 2          = mkAbsAddrIns (JPCC (condC y)) sys pc
  | z == 3          = case y of
                        0          -> mkAbsAddrIns JP sys pc
                        -- CB instruction prefix
                        1          -> let (newpc, (nextOpc, sys')) = mIncPCAndRead pc sys
                                          (bitIns, sys'')          = bitopsDecode sys' newpc nextOpc
                                      in  (DecodedInsn (newpc + 1) bitIns, sys'')
                        2          -> let (newpc, (nextOpc, sys')) = mIncPCAndRead pc sys
                                      in  (DecodedInsn (newpc + 1) ((OUT . PortImm) nextOpc), sys')
                        3          -> let (newpc, (nextOpc, sys')) = mIncPCAndRead pc sys
                                      in  (DecodedInsn (newpc + 1) ((IN . PortImm) nextOpc), sys')
                        4          -> defResult (EXC SPHL)
                        5          -> defResult (EXC DEHL)
                        6          -> defResult DI
                        7          -> defResult EI
                        _otherwise -> undefined
  | z == 4           = mkAbsAddrIns (CALLCC (condC y)) sys pc
  | z == 5           = case q of
                         0 -> defResult (PUSH (pairAF reg16XFormF p))
                         1 -> case p of
                                0          -> mkAbsAddrIns CALL sys pc
                                -- DD instruction prefix (should never reach here.)
                                1          -> undefined
                                -- ED instruction prefix
                                2          -> let (newpc, (nextOpc, sys')) = mIncPCAndRead pc sys
                                              in  edPrefixDecode nextOpc sys' newpc
                                -- FD instruction prefix (should never reach here.)
                                3          -> undefined
                                _otherwise -> undefined
                         _otherwise -> undefined
  | z == 6           = let (newpc, (nextOpc, sys')) = mIncPCAndRead pc sys
                           imm     = ALUimm nextOpc
                           insCTor = case y of
                                       0          -> ADD . ALU8
                                       1          -> ADC . ALU8
                                       2          -> SUB
                                       3          -> SBC . ALU8
                                       4          -> AND
                                       5          -> XOR
                                       6          -> OR
                                       7          -> CP
                                       _otherwise -> undefined
                       in  (DecodedInsn (newpc + 1) (insCTor imm), sys')
  | z == 7           = defResult (RST (y * 8))
  | otherwise        = defResult (Z80undef [opc])
  where
    z             = opc .&. 7
    y             = shiftR opc 3 .&. 7
    p             = shiftR opc 4 .&. 3
    q             = shiftR opc 3 .&. 1
    reg16XFormF   = reg16XForm xForm
    defResult ins = (DecodedInsn (pc + 1) ins, sys)

-- | The SET, RESet, BIT instructions and rotation operations. Note that this is not suitable for dealing with the
-- IX and IY indexed instructions, since the instruction format is 'DDCB <displacement> <opcode>', and has to be
-- handled seperately.
bitopsDecode :: Z80system sysType
             -- ^ Z80 memory (required to use the null register transform)
             -> Z80PC
             -- ^ Disassembly state, queried for current program counter
             -> Z80word
             -- ^ Bit/rotate operation opcode
             -> (Z80instruction, Z80system sysType)
             -- ^ The result
bitopsDecode sys pc opc =
  case shiftR opc 6 .&. 3 of
    0          -> ((rotOps ! fromIntegral y) theReg, sys')
    1          -> (BIT y theReg, sys')
    2          -> (RES y theReg, sys')
    3          -> (SET y theReg, sys')
    _otherwise -> undefined
  where
    z = opc .&. 7
    y = shiftR opc 3 .&. 7
    -- Ignore any new program counter 'reg8' because we always apply the null transform
    (_, theReg, sys') = reg8 (reg8XForm z80nullTransform) sys pc z

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

-- | Undocumented rotate/shift/bit operations using IX and IY
undocBitOpsDecode :: Z80word
                  -- ^ Original prefix, 0xdd or 0xfd
                  -> Z80PC
                  -- ^ Current program counter
                  -> Z80system sysType
                  -- ^ The Z80 system
                  -> Z80decodedInsn sysType
undocBitOpsDecode prefixOpc pc sys = let (pcStep1, (disp, sys')) = mIncPCAndRead pc sys
                                         (pcStep2, (opc, sys'')) = mIncPCAndRead pcStep1 sys'
                                         idxReg8Ctor     = (case prefixOpc of
                                                              0xdd       -> IXindirect
                                                              0xfd       -> IYindirect
                                                              _otherwise -> undefined) . fromIntegral
                                         z = opc .&. 7
                                         y = shiftR opc 3 .&. 7
                                         -- Ignore any new program counter 'reg8' because we always apply the null transform
                                         (_, theReg, sys''') = reg8 (reg8XForm z80nullTransform) sys'' pc z
                                     in  (DecodedInsn (pcStep2 + 1)
                                                      (case shiftR opc 6 .&. 3 of
                                                        0          -> (undocRotOps ! fromIntegral y) (idxReg8Ctor disp) theReg
                                                        1          -> BITidx y (idxReg8Ctor disp) theReg
                                                        2          -> RESidx y (idxReg8Ctor disp) theReg
                                                        3          -> SETidx y (idxReg8Ctor disp) theReg
                                                        _otherwise -> undefined
                                                      )
                                          , sys'''
                                          )

undocRotOps :: IntMap (Z80reg8 -> Z80reg8 -> Z80instruction)
undocRotOps = IntMap.fromList [ (0, RLCidx)
                              , (1, RRCidx)
                              , (2, RLidx)
                              , (3, RRidx)
                              , (4, SLAidx)
                              , (5, SRAidx)
                              , (6, SLLidx)
                              , (7, SRLidx)
                              ]

-- | Decode 'ED'-prefixed instructions
edPrefixDecode :: Z80word
               -> Z80system sysType
               -> Z80PC
               -> Z80decodedInsn sysType
edPrefixDecode opc sys pc
  | x == 0 = invalid
  | x == 1, z == 0, y /= 6 = let (newpc, reg, sys') = reg8 nullXFormF sys pc y
                             in  (DecodedInsn (newpc + 1) (IN . CIndIO $ reg), sys')
  | x == 1, z == 0, y == 6 = defResult (IN CIndIO0)
  | x == 1, z == 1, y /= 6 = let (newpc, reg, sys') = reg8 nullXFormF sys (newpc + 1) y
                             in  (DecodedInsn (newpc + 1) ((OUT . CIndIO) reg), sys')
  | x == 1, z == 1, y == 6 = defResult (OUT CIndIO0)
  | x == 1, z == 2, q == 0 = defResult ((SBC . ALU16) $ pairSP nullReg16XFormF p)
  | x == 1, z == 2, q == 1 = defResult ((ADC . ALU16) $ pairSP nullReg16XFormF p)
  | x == 1, z == 2         = invalid
  | x == 1, z == 3, q == 0 = mkAbsAddrIns (LD . RPIndirectStore (pairSP nullReg16XFormF p)) sys (pc + 1)
  | x == 1, z == 3, q == 1 = mkAbsAddrIns (LD . RPIndirectLoad (pairSP nullReg16XFormF p)) sys (pc + 1)
  | x == 1, z == 3         = invalid
  | x == 1, z == 4         = defResult NEG
  | x == 1, z == 5, y == 1 = defResult RETI
  | x == 1, z == 5, y /= 1 = defResult RETN
  | x == 1, z == 6         = defResult (IM (interruptMode ! fromIntegral y))
  | x == 1, z == 7         = case y of
                               0          -> defResult (LD IRegAcc)
                               1          -> defResult (LD RRegAcc)
                               2          -> defResult (LD AccIReg)
                               3          -> defResult (LD AccRReg)
                               4          -> defResult RLD
                               5          -> defResult RRD
                               6          -> invalid
                               7          -> invalid
                               _otherwise -> error ("edPrefixDecode, x = 1, z = 7: invalid y = " ++ show y)
  | x == 2, z <= 3, y >= 4 = -- Increment, Increment-Repeat instructions
                             defResult $ (incdecOps ! fromIntegral y) ! fromIntegral z
  | x == 2                 = invalid
  | x == 3                 = invalid
  -- Should never be matched...
  | otherwise              = error ("edPrefixDecode: could not decode instruction, x == "
                                    ++ show x
                                    ++ ", z == "
                                    ++ show z
                                    ++ ", y == "
                                    ++ show y)
  where
    x               = opc `shiftR` 6 .&. 3
    y               = opc `shiftR` 3 .&. 7
    z               = opc .&. 7
    p               = y `shiftR` 1 .&. 3
    q               = y .&. 1
    invalid         = defResult (Z80undef [0xed, opc])
    nullXFormF      = reg8XForm z80nullTransform
    nullReg16XFormF = reg16XForm z80nullTransform
    defResult ins   = (DecodedInsn (pc + 1) ins, sys)

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
pairSP _      x = error ("pairSP: invalid 16-bit register code " ++ show x)

-- | Convert 16-bit register pair/SP index to a 'RegPairSP' operand
pairAF :: (Z80reg16 -> Z80reg16)                -- ^ Transform function, when needed
       -> Z80word                               -- ^ Register pair index
       -> RegPairAF                             -- ^ Resulting register pair
pairAF _xform 0 = AFPair16 BC
pairAF _xform 1 = AFPair16 DE
pairAF  xform 2 = (AFPair16 . xform) HL
pairAF _xform 3 = AF
pairAF _      x = error ("pairAF: invalid 16-bit register code " ++ show x)

-- | Convert 8-bit register index to a 'Z80reg8' operand
reg8 :: Z80reg8XForm sysType
     -- ^ Register transform function, used for IX/IY transformations
     -> Z80system sysType
     -- ^ Memory from which to fetch IX/IY displacements
     -> Z80PC
     -- ^ Disassembler state
     -> Z80word
     -- ^ 8-bit register index
     -> (Z80PC, Z80reg8, Z80system sysType)
     -- ^ Register and new disassembly state
reg8 _xform sys  pc 0 = (pc, B, sys)
reg8 _xform sys  pc 1 = (pc, C, sys)
reg8 _xform sys  pc 2 = (pc, D, sys)
reg8 _xform sys  pc 3 = (pc, E, sys)
reg8 _xform sys  pc 4 = (pc, H, sys)
reg8 _xform sys  pc 5 = (pc, L, sys)
reg8  xform sys  pc 6 = xform sys pc HLindirect
reg8 _xform sys  pc 7 = (pc, A, sys)
reg8 _xform _sys pc x = error ("reg8: Invalid 8-bit register code " ++ show x ++ " at " ++ as0xHexS pc)

-- | Convert an 8-bit register index to an ALU operand 'OperALU'
--
-- This uses Control.Lens to apply the 'ALUreg8' data constructor on the first element of the pair
-- returned by 'reg8'

aluReg8 :: Z80reg8XForm sysType
        -- ^ IX/IY transform function, when needed
        -> Z80system sysType
        -- ^ Memory from which IX/IY displacements are fetched
        -> Z80PC
        -- ^ Current program counter
        -> Z80word
        -- ^ Register code
        -> (Z80PC, OperALU, Z80system sysType)

aluReg8 xform sys pc val = _2 %~ ALUreg8 $ reg8 xform sys pc val

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
condC x = error ("condC: Invalid condition code index " ++ show x)

-- | Compute signed, relative displacement address' absolute address
displacementInstruction :: Z80system sysType
                        -> Z80PC
                        -> (SymAbsAddr Z80addr -> Z80instruction)
                        -> Z80decodedInsn sysType
displacementInstruction sys pc ins =
  let (_, (disp, sys'))  = mIncPCAndRead pc sys
      destAddr             = fromIntegral (pc + signExtend disp + 2)
  in  (DecodedInsn (pc + 1) (ins . AbsAddr $ destAddr), sys')

-- | Fetch address, insert into an instruction
mkAbsAddrIns :: (SymAbsAddr Z80addr -> Z80instruction)
             -> Z80system sysType
             -> Z80PC
             -> Z80decodedInsn sysType
mkAbsAddrIns ins sys pc =
  let (DecodedAddr pc' addr, sys') = z80getAddr sys (pc + 1)
  in  (DecodedInsn pc' ((ins . AbsAddr) addr), sys')

-- | Fetch an absolute (16-bit) little endian address
z80getAddr :: Z80system sysType
           -- ^ Memory from which address is fetched
           -> Z80PC
           -- ^ The program counter
           -> Z80decodedInsn sysType
z80getAddr sys pc = let lo, hi     :: Z80word
                        (lo, sys')  = mRead (unPC pc) sys
                        (hi, sys'') = mRead (unPC (pc + 1)) sys'
                    in  (DecodedAddr (pc + 2) (shiftL (fromIntegral hi) 8 .|. fromIntegral lo), sys'')

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Index register transform functions:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Shorthand for 8-bit register transforms
type Z80reg8XForm sysType = Z80system sysType
                            -- The memory system
                            -> Z80PC
                            -- Program counter
                            -> Z80reg8
                            -- Register to be transformed
                            -> (Z80PC, Z80reg8, Z80system sysType)
                            -- Possibly incremented program counter, transformed register pair tuple

-- | Shorthand for 16-bit register transform
type Z80reg16XForm = (  Z80reg16                -- Register pair to be transformed
                     -> Z80reg16)               -- Resulting transformed register pair

-- | Transform the 8-bit register operand to the IX register and displacement, only
-- if the operand is indirect via HL. Also include the cases where H and L registers
-- are in the operand (undocumented instructions.)
ixXFormReg8 :: Z80reg8XForm sysType
ixXFormReg8 sys pc HLindirect = let (pc', (disp, sys')) = mIncPCAndRead pc sys
                                in  (pc', IXindirect (fromIntegral disp), sys')
ixXFormReg8 sys pc H          = (pc, IXh, sys)
ixXFormReg8 sys pc L          = (pc, IXl, sys)
ixXFormReg8 sys pc operand    = (pc, operand, sys)

-- | Transform the 8-bit register operand to the IY register and displacement, only
-- if the operand is indirect via HL
iyXFormReg8 :: Z80reg8XForm sysType
iyXFormReg8 sys   pc HLindirect = let (pc', (disp, sys')) = mReadAndIncPC pc sys
                                  in  (pc', IYindirect (fromIntegral disp), sys')
iyXFormReg8 sys pc H            = (pc, IYh, sys)
iyXFormReg8 sys pc L            = (pc, IYl, sys)
iyXFormReg8 sys pc operand      = (pc, operand, sys)

-- | Transform 16-bit register operands to an index register, only if HL happens
-- to be the destination. Used when decoding 0xdd prefixed instructions

ixXFormReg16 :: Z80reg16XForm
ixXFormReg16 HL    = IX
ixXFormReg16 other = other

-- | See 'ixXFormReg16' documentation -- this is for the IY register
iyXFormReg16 ::Z80reg16XForm
iyXFormReg16 HL    = IY
iyXFormReg16 other = other

-- | A collection of register transforms. Note that access to individual elements of
-- the record is mediated via 'Data.Label' lenses.
data Z80indexTransform sysType =
  Z80indexTransform
  { reg8XForm  :: Z80reg8XForm sysType
               -- Z80reg8 8-bit register transform
  , reg16XForm :: Z80reg16XForm
               -- RegPairSP and RegPairAF 16-bit register transform
  }

-- | Pass-through register transform: no transform required
z80nullTransform :: Z80indexTransform sysType
z80nullTransform = Z80indexTransform
                   { reg8XForm = \z80sys pc operand -> (pc, operand, z80sys)
                   , reg16XForm = id
                   }

-- | HL -> IX register transform collection
z80ixTransform :: Z80indexTransform sysType
z80ixTransform = Z80indexTransform
                 { reg8XForm = ixXFormReg8
                 , reg16XForm = ixXFormReg16
                 }

-- | HL -> IY register transform collection
z80iyTransform :: Z80indexTransform sysType
z80iyTransform = Z80indexTransform
                 { reg8XForm = iyXFormReg8
                 , reg16XForm = iyXFormReg16
                 }
