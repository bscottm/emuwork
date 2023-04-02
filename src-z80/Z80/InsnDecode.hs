{-# LANGUAGE GADTs                 #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Z80.InsnDecode
  ( -- * Types
    Z80decodedInsn

    -- * Functions
  , z80InsDecode
  , z80getAddr
  ) where

import           Lens.Micro.Platform
import           Data.Bits
import           Data.IntMap         (IntMap, (!))
import qualified Data.IntMap         as IntMap
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector.Unboxed as DVU

-- import Debug.Trace

import           Machine
import           Z80.InstructionSet
import           Z80.Processor
import           Z80.System

-- | 'DecodedInsn' shorthand for decoded Z80 instructions and resulting memory system
type Z80decodedInsn sysType = (DecodedInsn Z80instruction Z80addr, Z80system sysType)


z80InsDecode :: ProcessorDecoder Z80state Z80instruction Z80addr Z80word
z80InsDecode pc z80sys =
  case sysMRead (thePC pc) z80sys of
        (0xdd, z80sys')  -> indexedPrefix 0xdd z80ixTransform z80sys'
        (0xfd, z80sys')  -> indexedPrefix 0xfd z80iyTransform z80sys'
        (opc,  z80sys')  -> decode opc z80sys' pc z80nullTransform
  where
      indexedPrefix prefix xForm idxSys =
        case sysIncPCAndRead pc idxSys of
          (pc', (0xcb, sys'))   -> undocBitOpsDecode prefix pc' sys'
          (pc', (newOpc, sys')) -> decode newOpc sys' pc' xForm
      -- N.B. The opcode 'x' is passed through to the decoding function
      decode x = decodeFunc x
        where
        decodeFunc =
          case (x `shiftR` 6) .&. 3 of
            0          -> group0decode
            1          -> group1decode
            2          -> group2decode
            3          -> group3decode
            _otherwise -> undefined

-- | Break the instruction opcode into its z, y, p and q components
--
-- Opcode bits look like (courtesy of http://z80.info/decoding):
--
-- >  7  6  5  4  3  2  1  0
-- > -----  -------  -------
-- >   x       y        z
-- >        ---- --
-- >          p   q
--
-- x: Bits 6, 7
-- y: Bits 3, 4, 5 (subdivided into p [bits 4, 5] and q [bit 3])
-- z: Bits 0, 1, 2
opcComponents :: Z80word
              -> (Z80word, Z80word, Z80word, Z80word)
opcComponents opc =
  let z = opc .&. 7
      y = shiftR opc 3 .&. 7
      p = y `shiftR` 1
      q = y .&. 1
  in (z, y, p, q)

-- | Wrap a result from one of the instruction decode functions
wrapDecode :: Z80instruction
             -> ProgramCounter Z80addr
             -> EmulatedSystem Z80state Z80instruction Z80addr Z80word
             -> Z80decodedInsn sysType
wrapDecode ins pc sys = (DecodedInsn (1+ pc) ins, sys)
{-# INLINEABLE wrapDecode #-}

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
group0decode opc sys pc xForm =
  case opcComponents opc of
    (0, 0, _, _) -> oneByteInsn NOP sys pc
    (0, 1, _, _) -> oneByteInsn (EXC AFAF') sys pc
    (0, 2, _, _) -> displacementInstruction sys pc DJNZ
    (0, 3, _, _) -> displacementInstruction sys pc JR
    (0, y, _, _) -> displacementInstruction sys pc (JRCC $ condCode (y - 4))
    (1, _, p, 0) -> mkAbsAddrIns sys pc $ LDr16imm . Reg16Imm (pairSP reg16XFormF p)
    (1, _, p, 1) -> oneByteInsn (ADD16 (alu16XFormF DestHL) (pairSP reg16XFormF p)) sys pc
    (2, _, 0, 0) -> oneByteInsn (LDAmem ToBCindirect) sys pc
    (2, _, 1, 0) -> oneByteInsn (LDAmem ToDEindirect) sys pc
    (2, _, 2, 0) -> mkAbsAddrIns sys pc $ LDr16mem . indirectStoreXFormF . FromReg16 (RPair16 HL)
    (2, _, 3, 0) -> mkAbsAddrIns sys pc $ LDAmem . AccToMem
    (2, _, 0, 1) -> oneByteInsn (LDAmem FromBCindirect) sys pc
    (2, _, 1, 1) -> oneByteInsn (LDAmem FromDEindirect) sys pc
    (2, _, 2, 1) -> mkAbsAddrIns sys pc $ LDr16mem . indirectLoadXFormF . ToReg16 (RPair16 HL)
    (2, _, 3, 1) -> mkAbsAddrIns sys pc $ LDAmem . AccFromMem
    (3, _, p, 0) -> oneByteInsn (INC16 (pairSP reg16XFormF p)) sys pc
    (3, _, p, 1) -> oneByteInsn (DEC16 (pairSP reg16XFormF p)) sys pc
    (4, y, _, _) -> reg8DecodedInsnIXYhl reg8XFormF sys pc y INC
    (5, y, _, _) -> reg8DecodedInsnIXYhl reg8XFormF sys pc y DEC
    (6, y, _, _) ->
      let (newpc, theReg, sys')     = reg8IXYhl reg8XFormF sys pc y
          (newpc', (immval, sys'')) = sysIncPCAndRead newpc sys'
      in  (DecodedInsn (1+ newpc') (LDr8imm (Reg8Imm theReg immval)), sys'')
    (7, y, _, _) -> wrapDecode (accumOps ! fromIntegral y) pc sys
    _otherwise    -> wrapDecode (Z80undef [opc]) pc sys
    where
      reg8XFormF           = reg8XForm xForm
      reg16XFormF          = reg16XForm xForm
      alu16XFormF          = alu16XForm xForm
      indirectStoreXFormF  = indirectStoreXForm xForm
      indirectLoadXFormF   = indirectLoadXForm xForm

oneByteInsn :: Z80instruction
            -> Z80system sysType
            -> Z80PC
            -> Z80decodedInsn sysType
oneByteInsn insn sys pc = (DecodedInsn (1+ pc) insn, sys)
{-# INLINEABLE oneByteInsn #-}


-- | Accumulator operations map
accumOps :: IntMap Z80instruction
accumOps = IntMap.fromList
  [ (0, RLCA)
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
group1decode opc sys pc xform =
  case opcComponents opc of
    -- LD HL, HL -> HALT
    (6, 6, _, _) -> (DecodedInsn (pc + 1) HALT, sys)
    (z, y, _, _) ->
      let reg8XFormF              = reg8XForm xform
          srcReg' = reg8NumToReg ! fromIntegral z
          dstReg' = reg8NumToReg ! fromIntegral y
          xformF' =
            if (srcReg' `elem` [H, L] && dstReg' == HLindirect)
               || (dstReg' `elem` [H, L] && srcReg' == HLindirect)
            then reg8
            else reg8IXYhl     
          (newpc, dstReg, sys')   = xformF' reg8XFormF sys pc y
          (newpc', srcReg, sys'') = xformF' reg8XFormF sys' newpc z
      in  (DecodedInsn (1+ newpc') (LDr8r8 (Reg8Reg8 dstReg srcReg)), sys'')

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
                     0          -> ADD8 . ALUAcc
                     1          -> ADC8 . ALUAcc
                     2          -> SUB8 . ALUAcc
                     3          -> SBC8 . ALUAcc
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
group3decode opc sys pc xForm = case opcComponents opc of
    (0, y, _, _) -> wrapDecode (RETCC $ condCode y) pc sys
    (1, _, p, 0) -> wrapDecode ((POP . pairAF reg16XFormF) p) pc sys
    (1, _, 0, 1) -> wrapDecode RET pc sys
    (1, _, 1, 1) -> wrapDecode (EXC Primes) pc sys
    (1, _, 2, 1) -> wrapDecode (instructionXFormF JPHL) pc sys
    (1, _, 3, 1) -> wrapDecode (instructionXFormF LDSPHL) pc sys
    (2, y, _, _) -> mkAbsAddrIns sys pc $ JPCC (condCode y)
    (3, 0, _, _) -> mkAbsAddrIns sys pc JP
    (3, 1, _, _) -> let (newpc, (nextOpc, sys')) = sysIncPCAndRead pc sys
                        (bitIns, sys'') = bitopsDecode sys' newpc nextOpc
                    in  (DecodedInsn (newpc + 1) bitIns, sys'')
    (3, 2, _, _) -> let (newpc, (nextOpc, sys')) = sysIncPCAndRead pc sys
                    in  (DecodedInsn (newpc + 1) ((OUT . PortImm) nextOpc), sys')
    (3, 3, _, _) -> let (newpc, (nextOpc, sys')) = sysIncPCAndRead pc sys
                    in  (DecodedInsn (newpc + 1) ((IN . PortImm) nextOpc), sys')
    (3, 4, _, _) -> wrapDecode (EXC $ exchangeXFormF SPHL) pc sys
    (3, 5, _, _) -> wrapDecode (EXC DEHL) pc sys
    (3, 6, _, _) -> wrapDecode DI pc sys
    (3, 7, _, _) -> wrapDecode EI pc sys
    (4, y, _, _) -> mkAbsAddrIns sys pc $ CALLCC (condCode y)
    (5, _, p, 0) -> wrapDecode (PUSH (pairAF reg16XFormF p)) pc sys
    (5, _, 0, 1) -> mkAbsAddrIns sys pc CALL
    (5, _, 1, 1) -> wrapDecode (Z80undef [opc]) pc sys
    (5, _, 2, 1) -> let (newpc, (nextOpc, sys')) = sysIncPCAndRead pc sys
                    in  edPrefixDecode nextOpc sys' newpc
    (5, _, 3, 1) -> wrapDecode (Z80undef [opc]) pc sys
    (5, 0, p, _) -> wrapDecode (PUSH (pairAF reg16XFormF p)) pc sys
    (5, 1, 0, _) -> mkAbsAddrIns sys pc CALL
    (5, 1, 2, _) -> let (newpc, (nextOpc, sys')) = sysIncPCAndRead pc sys
                    in  edPrefixDecode nextOpc sys' newpc
    (5, _, _, _) -> wrapDecode (Z80undef [opc]) pc sys
    (6, 0, _, _) -> aluImm (ADD8 . ALUAcc)
    (6, 1, _, _) -> aluImm (ADC8 . ALUAcc)
    (6, 2, _, _) -> aluImm (SUB8 . ALUAcc)
    (6, 3, _, _) -> aluImm (SBC8 . ALUAcc)
    (6, 4, _, _) -> aluImm AND
    (6, 5, _, _) -> aluImm XOR
    (6, 6, _, _) -> aluImm OR
    (6, 7, _, _) -> aluImm CP
    (7, y, _, _) -> wrapDecode (RST (y * 8)) pc sys
    _            -> wrapDecode (Z80undef [opc]) pc sys
    where
      reg16XFormF   = reg16XForm xForm
      exchangeXFormF = exchangeXForm xForm
      instructionXFormF  = instructionXForm xForm
      aluImm insCTor =
        let (newpc, (nextOpc, sys')) = sysIncPCAndRead pc sys
            imm = ALUimm nextOpc
        in (DecodedInsn (newpc + 1) (insCTor imm), sys')

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
  case x of
    0          -> ((rotOps ! fromIntegral y) theReg, sys')
    1          -> (BIT y theReg, sys')
    2          -> (RES y theReg, sys')
    3          -> (SET y theReg, sys')
    _otherwise -> undefined
  where
    x              = opc `shiftR` 6 .&. 3
    (z, y, _p, _q) = opcComponents opc
    -- Ignore any new program counter 'reg8' because we always apply the null transform
    (_, theReg, sys') = reg8 (reg8XForm z80nullTransform) sys pc z

rotOps :: IntMap (Z80reg8 -> Z80instruction)
rotOps = IntMap.fromList
  [ (0, RLC)
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
undocBitOpsDecode prefixOpc pc sys =
  let (pcStep1, (disp, sys')) = sysIncPCAndRead pc sys
      (pcStep2, (opc, sys'')) = sysIncPCAndRead pcStep1 sys'
      idxReg8Ctor     = (case prefixOpc of
                          0xdd       -> IXindirect
                          0xfd       -> IYindirect
                          _otherwise -> undefined) . fromIntegral
      x = shiftR opc 6 .&. 3
      (z, y, _p, _q) = opcComponents opc
      -- Ignore any new program counter 'reg8' because we always apply the null transform
      (_, theReg, sys''') = reg8 (reg8XForm z80nullTransform) sys'' pc z
      instruction
        | theReg == HLindirect
        = case x of
            0          -> (rotOps ! fromIntegral y) (idxReg8Ctor disp)
            1          -> BIT y (idxReg8Ctor disp)
            2          -> RES y (idxReg8Ctor disp)
            3          -> SET y (idxReg8Ctor disp)
            _otherwise -> undefined
        | otherwise
        = case x of
            0          -> (undocRotOps ! fromIntegral y) (idxReg8Ctor disp) theReg
            1          -> BIT y (idxReg8Ctor disp)
            2          -> RESidx y (idxReg8Ctor disp) theReg
            3          -> SETidx y (idxReg8Ctor disp) theReg
            _otherwise -> undefined
  in  (DecodedInsn (pcStep2 + 1) instruction, sys''')

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
edPrefixDecode opc sys pc = case (x, z, y, p, q) of
  (0, _, _, _, _) -> invalid
  (1, 0, 6, _, _) -> wrapDecode (IN CIndIO0) pc sys
  (1, 0, _, _, _) -> wrapDecode (IN . CIndIO $ (reg8NumToReg ! fromIntegral y)) pc sys
  (1, 1, 6, _, _) -> wrapDecode (OUT CIndIO0) pc sys
  (1, 1, _, _, _) -> wrapDecode (OUT . CIndIO $ (reg8NumToReg ! fromIntegral y)) pc sys
  (1, 2, _, _, 0) -> wrapDecode (SBC16 (alu16XFormF DestHL) $ pairSP nullReg16XFormF p) pc sys
  (1, 2, _, _, 1) -> wrapDecode (ADC16 (alu16XFormF DestHL) $ pairSP nullReg16XFormF p) pc sys
  (1, 2, _, _, _) -> invalid
  (1, 3, _, _, 0) -> mkAbsAddrIns sys pc $ LDr16mem . FromReg16 (pairSP nullReg16XFormF p)
  (1, 3, _, _, 1) -> mkAbsAddrIns sys pc $ LDr16mem . ToReg16 (pairSP nullReg16XFormF p)
  (1, 3, _, _, _) -> invalid
  (1, 4, _, _, _) -> wrapDecode NEG pc sys
  (1, 5, _, _, 1) -> wrapDecode RETI pc sys
  (1, 5, _, _, 0) -> wrapDecode RETN pc sys
  (1, 6, _, _, _) -> wrapDecode (IM (interruptMode ! fromIntegral y)) pc sys
  (1, 7, 0, _, _) -> wrapDecode (LDAspecial FromAtoI) pc sys
  (1, 7, 1, _, _) -> wrapDecode (LDAspecial FromAtoR) pc sys
  (1, 7, 2, _, _) -> wrapDecode (LDAspecial FromItoA) pc sys
  (1, 7, 3, _, _) -> wrapDecode (LDAspecial FromRtoA) pc sys
  (1, 7, 4, _, _) -> wrapDecode RRD pc sys
  (1, 7, 5, _, _) -> wrapDecode RLD pc sys
  (1, 7, 6, _, _) -> invalid
  (1, 7, 7, _, _) -> invalid
  (2, _, _, _, _) ->
    if z <= 3 && y >= 4
    then
      -- Increment, Increment-Repeat instructions
        wrapDecode ((incdecOps ! fromIntegral y) ! fromIntegral z) pc sys
    else
        invalid
  (3, _, _, _, _) -> invalid
  -- Catch-all for Haskell's incomplete pattern matching
  (_, _, _, _, _) -> invalid
  where
    x               = opc `shiftR` 6 .&. 3
    (z, y, p, q)    = opcComponents opc
    invalid         = wrapDecode (Z80undef [0xed, opc]) pc sys
    nullReg16XFormF = reg16XForm z80nullTransform
    alu16XFormF     = alu16XForm z80nullTransform

-- | Block/compare/input/output increment-decrement lookup table
incdecOps :: IntMap (IntMap Z80instruction)
incdecOps = IntMap.fromList
  [ (4, IntMap.fromList [ (0, LDI )
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
interruptMode = IntMap.fromList
  [ (0, 0)
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

-- | Convert 8-bit register index to a 'Z80reg8' operand. Note that this
-- will leave H and L untransformed to avoid "LD IXh, (IX+3)" transformations.
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
reg8 xform sys pc 6 = xform sys pc HLindirect
reg8 _ sys pc n = (pc, reg, sys) where
    reg = fromMaybe (error $ "reg8: Invalid register index: " ++ show n)
                    (IntMap.lookup (fromIntegral n) reg8NumToReg)

reg8NumToReg :: IntMap Z80reg8
reg8NumToReg = IntMap.fromList
  [ (0, B)
  , (1, C)
  , (2, D)
  , (3, E)
  , (4, H)
  , (5, L)
  , (6, HLindirect)
  , (7, A)
  ]

-- | Convert 8-bit register index to a 'Z80reg8' operand.
reg8IXYhl :: Z80reg8XForm sysType
     -- ^ Register transform function, used for IX/IY transformations
     -> Z80system sysType
     -- ^ Memory from which to fetch IX/IY displacements
     -> Z80PC
     -- ^ Disassembler state
     -> Z80word
     -- ^ 8-bit register index
     -> (Z80PC, Z80reg8, Z80system sysType)
     -- ^ Register and new disassembly state
reg8IXYhl xform sys pc 4 = xform sys pc H
reg8IXYhl xform sys pc 5 = xform sys pc L
reg8IXYhl xform sys pc 6 = xform sys pc HLindirect
reg8IXYhl _ sys pc n = (pc, reg, sys) where
    reg = case n of
        0 -> B
        1 -> C
        2 -> D
        3 -> E
        7 -> A
        _ -> error "reg8IXYhl: Invalid register number"

{-
-- | Convert 8-bit register index to a 'Z80reg8' operand, prohibiting
-- transforms H -> I[XY]h and L -> I[XY]l
reg8DecodedInsn :: Z80reg8XForm sysType
                -- ^ Register transform function, used for IX/IY transformations
                -> Z80system sysType
                -- ^ Memory from which to fetch IX/IY displacements
                -> Z80PC
                -- ^ Current program counter
                -> Z80word
                -- ^ 8-bit register index
                -> (Z80reg8 -> Z80instruction)
                -- ^ Instruction constructor
                -> Z80decodedInsn sysType
                -- ^ Register and new disassembly state

reg8DecodedInsn xform sys pc regWord ctor =
  let (newpc, theReg, sys')     = reg8 xform sys pc regWord
  in  (DecodedInsn (newpc + 1) (ctor theReg), sys')
-}

-- | Convert 8-bit register index to a 'Z80reg8' operand, allowing
-- transforms H -> I[XY]h and L -> I[XY]l
reg8DecodedInsnIXYhl :: Z80reg8XForm sysType
                     -- ^ Register transform function, used for IX/IY transformations
                     -> Z80system sysType
                     -- ^ Memory from which to fetch IX/IY displacements
                     -> Z80PC
                     -- ^ Current program counter
                     -> Z80word
                     -- ^ 8-bit register index
                     -> (Z80reg8 -> Z80instruction)
                     -- ^ Instruction constructor
                     -> Z80decodedInsn sysType
                     -- ^ Register and new disassembly state
reg8DecodedInsnIXYhl xform sys pc regWord ctor =
  let (newpc, theReg, sys')     = reg8IXYhl xform sys pc regWord
  in  (DecodedInsn (newpc + 1) (ctor theReg), sys')

-- | Convert an 8-bit register index to an ALU operand 'OperALU'
aluReg8 :: Z80reg8XForm sysType
        -- ^ IX/IY transform function, when needed
        -> Z80system sysType
        -- ^ Memory from which IX/IY displacements are fetched
        -> Z80PC
        -- ^ Current program counter
        -> Z80word
        -- ^ Register code
        -> (Z80PC, OperALU, Z80system sysType)
-- Use the _2 tuple lens to apply the 'ALUreg8' data constructor on the first element of the pair
-- returned by 'reg8'
aluReg8 xform sys pc val = _2 %~ ALUreg8 $ reg8IXYhl xform sys pc val

-- | Convert condition code
condCode :: Z80word
      -> Z80condC
condCode 0 = NZ
condCode 1 = Z
condCode 2 = NC
condCode 3 = CY
condCode 4 = PO
condCode 5 = PE
condCode 6 = POS
condCode 7 = MI
condCode x = error ("condCode: Invalid condition code index " ++ show x)

-- | Compute signed, relative displacement address' absolute address
displacementInstruction :: Z80system sysType
                        -> Z80PC
                        -> (SymAbsAddr Z80addr -> Z80instruction)
                        -> Z80decodedInsn sysType
displacementInstruction sys pc ins =
  let (_, (disp, sys'))  = sysIncPCAndRead pc sys
      destAddr           = fromIntegral (pc + signExtend disp + 2)
  in  (DecodedInsn (pc + 2) (ins . AbsAddr $ destAddr), sys')

-- | Fetch address, insert into an instruction
mkAbsAddrIns :: Z80system sysType
             -> Z80PC
             -> (SymAbsAddr Z80addr -> Z80instruction)
             -> Z80decodedInsn sysType
mkAbsAddrIns sys pc ins = z80getAddr sys (pc + 1) & _1 %~ xformAddr
  where
    xformAddr addr = DecodedInsn (pc + 3) $ ins . AbsAddr $ addr

-- | Fetch an absolute (16-bit) little endian address
z80getAddr :: Z80system sysType
           -- ^ Memory from which address is fetched
           -> Z80PC
           -- ^ The program counter
           -> (Z80addr, Z80system sysType)
z80getAddr sys pc = sysMReadN (thePC pc) 2 sys & _1 %~ makeAddr
  where
    makeAddr awords = shiftL (fromIntegral (awords DVU.! 1)) 8 .|. fromIntegral (awords DVU.! 0)

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
type Z80reg16XForm =    Z80reg16               -- Register pair to be transformed
                     -> Z80reg16               -- Resulting transformed register pair

-- | Shorthand for the special case 16-bit ALU register transforms
type Z80ALU16XForm =    DestALU16
                     -> DestALU16

-- | Indirect stores: LD (addr), [HL|IX|IY]
type Z80IndirectStoreXForm = Reg16Mem
                           -> Reg16Mem

-- | Indirect loads: LD [HL|IX|IY], (addr)
type Z80IndirectLoadXForm = Reg16Mem
                          -> Reg16Mem

-- | Exchange operands
type Z80ExchangeXForm     = Z80ExchangeOper
                          -> Z80ExchangeOper

-- | Misc instruction transforms
type Z80InstructionXForm  = Z80instruction
                          -> Z80instruction

-- | Transform the 8-bit register operand to the IX register and displacement, only
-- if the operand is indirect via HL. Also include the cases where H and L registers
-- are in the operand (undocumented instructions.)
ixXFormReg8 :: Z80reg8XForm sysType
ixXFormReg8 sys pc HLindirect = let (pc', (disp, sys')) = sysIncPCAndRead pc sys
                                in  (pc', IXindirect (fromIntegral disp), sys')
ixXFormReg8 sys pc H          = (pc, IXh, sys)
ixXFormReg8 sys pc L          = (pc, IXl, sys)
ixXFormReg8 sys pc operand    = (pc, operand, sys)

-- | Transform the 8-bit register operand to the IY register and displacement, only
-- if the operand is indirect via HL
iyXFormReg8 :: Z80reg8XForm sysType
iyXFormReg8 sys pc HLindirect = let (pc', (disp, sys')) = sysIncPCAndRead pc sys
                                in  (pc', IYindirect (fromIntegral disp), sys')
iyXFormReg8 sys pc H            = (pc, IYh, sys)
iyXFormReg8 sys pc L            = (pc, IYl, sys)
iyXFormReg8 sys pc operand      = (pc, operand, sys)

-- | Transform 16-bit register operands to an index register, only if HL happens
-- to be the destination. Used when decoding 0xdd prefixed instructions

ixXFormReg16 :: Z80reg16XForm
ixXFormReg16 HL    = IX
ixXFormReg16 other = other

-- | Transform 16-bit ALU operands where HL is the destination.
ixXFormALU16 :: Z80ALU16XForm
ixXFormALU16 DestHL = DestIX
ixXFormALU16 other  = other

-- | Transform 16-bit indirect stores, e.g., "LD (aaaa), HL" when HL is the
-- source.
ixXFormIndirectStore :: Z80IndirectStoreXForm
ixXFormIndirectStore (FromReg16 (RPair16 HL) addr) = FromReg16 (RPair16 IX) addr
ixXFormIndirectStore other                         = other

ixXFormIndirectLoad :: Z80IndirectLoadXForm
ixXFormIndirectLoad (ToReg16 (RPair16 HL) addr)    = ToReg16 (RPair16 IX) addr
ixXFormIndirectLoad other                          = other

ixXFormExchange :: Z80ExchangeXForm
ixXFormExchange SPHL  = SPIX
ixXFormExchange other = other

ixXFormInstruction :: Z80InstructionXForm
ixXFormInstruction JPHL = JPIX
ixXFormInstruction LDSPHL = LDSPIX
ixXFormInstruction other = other

-- | See 'ixXFormReg16' documentation -- this is for the IY register
iyXFormReg16 ::Z80reg16XForm
iyXFormReg16 HL    = IY
iyXFormReg16 other = other

iyXFormALU16 :: Z80ALU16XForm
iyXFormALU16 DestHL = DestIY
iyXFormALU16 other  = other

iyXFormIndirectStore :: Z80IndirectStoreXForm
iyXFormIndirectStore (FromReg16 (RPair16 HL) addr) = FromReg16 (RPair16 IY) addr
iyXFormIndirectStore other                         = other

iyXFormIndirectLoad :: Z80IndirectLoadXForm
iyXFormIndirectLoad (ToReg16 (RPair16 HL) addr)   = ToReg16 (RPair16 IY) addr
iyXFormIndirectLoad other                         = other

iyXFormExchange :: Z80ExchangeXForm
iyXFormExchange SPHL  = SPIY
iyXFormExchange other = other

iyXFormInstruction :: Z80InstructionXForm
iyXFormInstruction JPHL = JPIY
iyXFormInstruction LDSPHL = LDSPIY
iyXFormInstruction other = other

-- | A collection of register transforms. Note that access to individual elements of
-- the record is mediated via 'Data.Label' lenses.
data Z80indexTransform sysType =
  Z80indexTransform
  { reg8XForm  :: Z80reg8XForm sysType
  -- ^ Z80reg8 8-bit register transform
  , reg16XForm :: Z80reg16XForm
  -- ^ RegPairSP and RegPairAF 16-bit register transform
  , alu16XForm :: Z80ALU16XForm
  -- ^ 16-bit ALU register transform
  , indirectStoreXForm :: Z80IndirectStoreXForm
  -- ^ Indirect store transform
  , indirectLoadXForm :: Z80IndirectLoadXForm
  -- ^ Indirect load transform
  , exchangeXForm :: Z80ExchangeXForm
  -- ^ Exchange operand transform
  , instructionXForm :: Z80InstructionXForm
  -- ^ Indirect jump transform
  }

-- | Pass-through register transform: no transform required
z80nullTransform :: Z80indexTransform sysType
z80nullTransform = Z80indexTransform
                   { reg8XForm = \z80sys pc operand -> (pc, operand, z80sys)
                   , reg16XForm = id
                   , alu16XForm = id
                   , indirectStoreXForm = id
                   , indirectLoadXForm = id
                   , exchangeXForm = id
                   , instructionXForm = id
                   }

-- | HL -> IX register transform collection
z80ixTransform :: Z80indexTransform sysType
z80ixTransform = Z80indexTransform
                 { reg8XForm = ixXFormReg8
                 , reg16XForm = ixXFormReg16
                 , alu16XForm = ixXFormALU16
                 , indirectStoreXForm = ixXFormIndirectStore
                 , indirectLoadXForm = ixXFormIndirectLoad
                 , exchangeXForm = ixXFormExchange
                 , instructionXForm = ixXFormInstruction
                 }

-- | HL -> IY register transform collection
z80iyTransform :: Z80indexTransform sysType
z80iyTransform = Z80indexTransform
                 { reg8XForm = iyXFormReg8
                 , reg16XForm = iyXFormReg16
                 , alu16XForm = iyXFormALU16
                 , indirectStoreXForm = iyXFormIndirectStore
                 , indirectLoadXForm = iyXFormIndirectLoad
                 , exchangeXForm = iyXFormExchange
                 , instructionXForm = iyXFormInstruction
                 }