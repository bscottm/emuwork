module Main where

import           Data.Bits           (Bits (..))
import           Data.Int
import           Data.IntMap         (IntMap, (!))
import qualified Data.IntMap         as IntMap
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector.Unboxed as DVU

import           Lens.Micro.Platform ((&), (.~), (^.), _1, (%~))

import           Machine

import           System.IO

import           Z80
import           Z80.Tests.InstData

-- import Debug.Trace

-- | Z80 opcode components: z, y, p and q
type OpcComponents = (Z80byte, Z80byte, Z80byte, Z80byte)
-- |
type OpLocation sysType = (Z80PC, Z80system sysType)
-- |
type TransformerFunc opndType sysType =
     opndType
  -- ^ An operand type, with or without 'Z80operand'
  -> OpLocation sysType
  -- ^ Location in memory where the opcode and operands live.
  -> (opndType, Z80PC, Z80system sysType)
-- |
type IndexTransformerFunc opndType sysType =
     IndexRegSubs
  -> TransformerFunc opndType sysType

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

zz_z80InsDecode :: ProcessorDecoder Z80state Z80instruction Z80addr Z80byte
zz_z80InsDecode pc z80sys =
  case sysMRead (thePC pc) z80sys of
        (0xdd, z80sys')  ->
          let (insn, endPC, endSys) = prefixed 0xdd ixSubstitutions z80sys'
          in  (DecodedInsn endPC insn, endSys)
        (0xfd, z80sys')  ->
          let (insn, endPC, endSys) = prefixed 0xfd iySubstitutions z80sys'
          in  (DecodedInsn endPC insn, endSys)
        (opc,  z80sys')  ->
          let (insn, endPC, endSys) = decode [] opc NoIndexTranslation (pc, z80sys')
          in  (DecodedInsn endPC insn, endSys)
  where
    prefixed prefix xForm idxSys =
      case sysIncPCAndRead pc idxSys of
        (pc', (0xcb, sys'))    -> undocBitOpsDecode prefix (pc', sys')
        (pc',  (newOpc, sys')) -> decode [prefix, newOpc] newOpc xForm (pc', sys')

    -- N.B. The opcode 'x' is passed through to the decoding function
    decode opcodes opc = decoderGroup opcodes opGroup (opcodeComponents opc)
      where
        opGroup = opcodeGroup opc
        decoderGroup =
          case opGroup of
            0          -> group0decode
            1          -> group1decode
            2          -> group2decode
            3          -> group3decode
            _otherwise -> error $ concat
               ["opcode "
               , as0xHexS opc
               , ", group: "
               , as0xHexS opGroup
               , ", components "
               , show (opcodeComponents opc)
               ]

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Break the opcode into its components
opcodeComponents :: Z80byte
              -> OpcComponents
opcodeComponents opc =
  let z = opc .&. 7
      y =  opc `shiftR` 3 .&. 7
      p = y `shiftR` 1
      q = y .&. 1
  in (z, y, p, q)

-- | Get the opcode group (aka "x")
opcodeGroup
  :: Z80byte
  -> Z80byte
opcodeGroup opc = opc `shiftR` 6 .&. 3

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

class IndexSubstitution opndType where
  indexSubstitute :: IndexTransformerFunc opndType sysType

instance IndexSubstitution Z80instruction where
  indexSubstitute NoIndexTranslation  opnd   (pc, sys) = (opnd, pc, sys)
  indexSubstitute subs                JPHL   (pc, sys) = (idxJPInsn subs, pc, sys)
  indexSubstitute subs                LDSPHL (pc, sys) = (idxLDSPInsn subs, pc, sys)
  indexSubstitute _subs               insn (pc, sys) = (insn, pc, sys)

instance IndexSubstitution Z80reg8 where
  indexSubstitute NoIndexTranslation  opnd       (pc, sys) = (opnd, pc, sys)
  indexSubstitute subs HLindirect (pc, sys) =
    let (disp, pc', sys') = readIndexByte pc sys
    in  (indirectCTor subs disp, pc', sys')
  indexSubstitute subs H    (pc, sys) = (hi8 subs, pc, sys)
  indexSubstitute subs L    (pc, sys) = (lo8 subs, pc, sys)
  indexSubstitute _subs reg8 (pc, sys) = (reg8, pc, sys)

instance IndexSubstitution RegPairSP where
  indexSubstitute NoIndexTranslation  opnd       (pc, sys) = (opnd, pc, sys)
  indexSubstitute subs (RPair16 HL) (pc, sys)  = (idxRegPairSP subs, pc, sys)
  indexSubstitute _subs opnd (pc, sys)         = (opnd, pc, sys)

instance IndexSubstitution RegPairAF where
  indexSubstitute NoIndexTranslation  opnd       (pc, sys) = (opnd, pc, sys)
  indexSubstitute subs (AFPair16 HL) (pc, sys)  = (idxRegPairAF subs, pc, sys)
  indexSubstitute _subs opnd (pc, sys)         = (opnd, pc, sys)

instance IndexSubstitution (Z80operand Z80OpndLoad) where
  indexSubstitute NoIndexTranslation  opnd       (pc, sys)       = (opnd, pc, sys)
  indexSubstitute subs (Reg8Reg8 reg HLindirect) (pc, sys)       =
    let (disp, pc', sys') = readIndexByte pc sys
    in  (Reg8Reg8 reg (indirectCTor subs disp), pc', sys')
  indexSubstitute subs (Reg8Reg8 HLindirect reg) (pc, sys)       =
    let (disp, pc', sys') = readIndexByte pc sys
    in  (Reg8Reg8 (indirectCTor subs disp) reg, pc', sys')
  indexSubstitute subs (Reg8Reg8 dst H) (pc, sys)                =
    let (dst_opnd, pc', sys') = indexSubstitute subs dst (pc, sys)
    in  (Reg8Reg8 dst_opnd (hi8 subs), pc', sys')
  indexSubstitute subs (Reg8Reg8 H src) (pc, sys)                =
    let (src_opnd, pc', sys') = indexSubstitute subs src (pc, sys)
    in  (Reg8Reg8 (hi8 subs) src_opnd, pc', sys')
  indexSubstitute subs (Reg8Reg8 dst L) (pc, sys)                =
    let (dst_opnd, pc', sys') = indexSubstitute subs dst (pc, sys)
    in  (Reg8Reg8 dst_opnd (lo8 subs), pc', sys')
  indexSubstitute subs (Reg8Reg8 L src) (pc, sys)                =
    let (src_opnd, pc', sys') = indexSubstitute subs src (pc, sys)
    in  (Reg8Reg8 (lo8 subs) src_opnd, pc', sys')
  indexSubstitute subs (Reg16Imm  (RPair16 HL) addr) (pc, sys)   = (Reg16Imm (RPair16 $ idxReg16 subs) addr, pc, sys)
  indexSubstitute _subs opnd (pc, sys)                           = (opnd, pc, sys)

instance IndexSubstitution (Z80operand Z80OpndLoad16) where
  indexSubstitute NoIndexTranslation  opnd       (pc, sys)       = (opnd, pc, sys)
  indexSubstitute subs (FromReg16 (RPair16 HL) addr) (pc, sys)   = (FromReg16 (RPair16 $ idxReg16 subs) addr, pc, sys)
  indexSubstitute subs (ToReg16 (RPair16 HL) addr) (pc, sys)     = (ToReg16 (RPair16 $ idxReg16 subs) addr, pc, sys)
  indexSubstitute _subs opnd (pc, sys)                           = (opnd, pc, sys)

instance IndexSubstitution (Z80operand Z80OpndALU16) where
  indexSubstitute NoIndexTranslation  opnd         (pc, sys) = (opnd, pc, sys)
  indexSubstitute subs                (DestHL src) (pc, sys) =
    -- Ensure that the index register gets substituted in the src as well.
    let (rpair_opnd, pc', sys') = indexSubstitute subs src (pc, sys)
    in  (idxALU16 subs rpair_opnd, pc', sys')
  indexSubstitute _subs               opnd         (pc, sys) = (opnd, pc, sys)

instance IndexSubstitution Z80ExchangeOper where
  indexSubstitute NoIndexTranslation  opnd    (pc, sys) = (opnd, pc, sys)
  indexSubstitute subs                SPHL    (pc, sys) = (idxEXSPOpnd subs, pc, sys)
  indexSubstitute _subs               opnd    (pc, sys) = (opnd, pc, sys)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data IndexRegSubs where
  NoIndexTranslation :: IndexRegSubs
  IndexRegSubs ::
    { hi8 :: Z80reg8
    , lo8 :: Z80reg8
    , idxReg16 :: Z80reg16
    , idxRegPairSP :: RegPairSP
    , idxRegPairAF :: RegPairAF
    , idxALU16 :: RegPairSP -> Z80operand Z80OpndALU16
    , idxJPInsn :: Z80instruction
    , idxLDSPInsn :: Z80instruction
    , idxEXSPOpnd :: Z80ExchangeOper
    , indirectCTor :: Int8 -> Z80reg8
    } -> IndexRegSubs

ixSubstitutions :: IndexRegSubs
ixSubstitutions =
  IndexRegSubs
    { hi8 = IXh
    , lo8 = IXl
    , idxReg16 = IX
    , idxRegPairSP = RPair16 IX
    , idxRegPairAF = AFPair16 IX
    , idxALU16 = DestIX
    , idxJPInsn = JPIX
    , idxLDSPInsn = LDSPIX
    , idxEXSPOpnd = SPIX
    , indirectCTor = IXindirect
    }

iySubstitutions :: IndexRegSubs
iySubstitutions =
  IndexRegSubs
    { hi8 = IYh
    , lo8 = IYl
    , idxReg16 = IY
    , idxRegPairSP = RPair16 IY
    , idxRegPairAF = AFPair16 IY
    , idxALU16 = DestIY
    , idxJPInsn = JPIY
    , idxLDSPInsn = LDSPIY
    , idxEXSPOpnd = SPIY
    , indirectCTor = IYindirect
    }

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

class Z80OperandDecoder opndType where
  decodeOperands
    :: IndexRegSubs
    -> Z80byte
    -> OpcComponents
    -> OpLocation sysType
    -> (Z80operand opndType, Z80PC, Z80system sysType)

instance Z80OperandDecoder Z80OpndALU16 where
  decodeOperands xform _opcGroup (_, _, p, _) {-place-} = indexSubstitute xform opnd
    where
      opnd = DestHL (mkRegPairSP p)

instance Z80OperandDecoder Z80OpndLoad where
  decodeOperands xform opcGroup opcParts place =
    case opcGroup of
      0 -> group0Load opcParts place
      1 -> group1Load opcParts place
      _ -> error ("Z80OpndLoad: opcGroup " ++ as0xHexS opcGroup ++ " parts " ++ show opcParts)
    where
      group0Load, group1Load
          :: OpcComponents
          -> OpLocation sysType
          -> (Z80operand Z80OpndLoad, Z80PC, Z80system sysType)

      group0Load (z, y, p, q) place'@(pc, sys) =
        case (z, p, q) of
          (1, _, 0) -> indexSubstitute xform `applyTransformer` read16BitValue (Reg16Imm (mkRegPairSP p)) place'
          (2, 0, 0) -> simple ToBCindirect
          (2, 1, 0) -> simple ToDEindirect
          (2, 3, 0) -> read16BitValue AccToMem place'
          (2, 0, 1) -> simple FromBCindirect
          (2, 1, 1) -> simple FromDEindirect
          (2, 3, 1) -> read16BitValue AccFromMem place'
          (6, _, _) -> read8BitValue (Reg8Imm theReg) (pc', sys')
            where
              (theReg, pc', sys') = indexSubstitute xform (reg8NumToReg ! fromIntegral y) place
          _ -> undefined
        where
          simple opnd = (opnd, 1+ pc, sys)

      group1Load (z, y, _, _) (pc, sys) =
        let -- loadSubst = substOpndLoads xform
            srcReg = reg8NumToReg ! fromIntegral z
            dstReg = reg8NumToReg ! fromIntegral y
        in  bumpPC $ indexSubstitute xform `applyTransformer` (Reg8Reg8 dstReg srcReg, pc, sys)

instance Z80OperandDecoder Z80OpndLoad16 where
  -- Group 0 are ordinary instruction group 0/group0decode
  -- Group 1 are 0xed prefixed instructions.
  decodeOperands  xform 0 (2, _, 2, 0) place = indexSubstitute xform `applyTransformer` read16BitValue (FromReg16 (RPair16 HL)) place
  decodeOperands  xform 0 (2, _, 2, 1) place = indexSubstitute xform `applyTransformer` read16BitValue (ToReg16 (RPair16 HL)) place
  decodeOperands _xform 1 (3, _, p, 0) place = read16BitValue (FromReg16 (mkRegPairSP p)) place
  decodeOperands _xform 1 (3, _, p, 1) place = read16BitValue (ToReg16 (mkRegPairSP p)) place
  decodeOperands _xform opcGroup opcParts _place = error ("Z80OpndLoad: opcGroup " ++ as0xHexS opcGroup ++ " parts " ++ show opcParts)

instance Z80OperandDecoder Z80OpndInc where
  decodeOperands xform _opcGroup (3, _, p, _) place =
    let (opnd, pc', sys') = indexSubstitute xform (mkRegPairSP p) place
    in  (IncDecReg16 opnd, 1+ pc', sys')
  decodeOperands xform _opcGroup (_, y, _, _) place =
    let (opnd, pc', sys') = indexSubstitute xform (reg8NumToReg ! fromIntegral y) place
    in  (IncDecReg8 opnd, 1+ pc', sys')

instance Z80OperandDecoder Z80OpndALU where
  decodeOperands xform 2 (z, _, _, _) place =
    let (opnd, pc', sys') = indexSubstitute xform (reg8NumToReg ! fromIntegral z) place
    in  (ALUreg8 opnd, 1+ pc', sys')
  decodeOperands _xform 3 _opcParts (pc, sys) = read8BitValue ALUimm (pc, sys)
  decodeOperands _xform opcGroup opcParts _place = error ("Z80OperandDecoder Z80OpndALU: group " ++ show opcGroup ++ ", " ++ show opcParts)

instance Z80OperandDecoder Z80OpndIO where
  -- opcGroup 1 is really from the 0xed prefix decoder
  decodeOperands _xform 1 (_, 6, _, _) (pc, sys) = (CIndIO0, 1+ pc, sys)
  decodeOperands _xform 1 (_, y, _, _) (pc, sys) = (CIndIO (reg8NumToReg ! fromIntegral y), 1+ pc, sys)
  -- opcGroup 3 is really from group3decode
  decodeOperands _xform 3 (3, _, _, _) opLocn    = read8BitValue PortImm opLocn
  decodeOperands _xform opcGroup opcParts _opLocn = error ("Z80OperandDecoder Z80OpndIO: group " ++ show opcGroup ++ ", " ++ show opcParts)


-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

applyTransformer
  :: TransformerFunc opndType sysType
  -> (opndType, Z80PC, Z80system sysType)
  -> (opndType, Z80PC, Z80system sysType)
applyTransformer f (opnd, pc, sys) = f opnd (pc, sys)

bumpPC
  :: (opndType, Z80PC, Z80system sysType)
  -> (opndType, Z80PC, Z80system sysType)
bumpPC (opnd, pc, sys) = (opnd, 1+ pc, sys)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Fetch an absolute (16-bit) little endian address
z80getAddr :: Z80system sysType
           -- ^ Memory from which address is fetched
           -> Z80PC
           -- ^ The program counter
           -> (Z80addr, Z80system sysType)
z80getAddr sys pc = sysMReadN (thePC pc) 2 sys & _1 %~ makeAddr
  where
    makeAddr awords = shiftL (fromIntegral (awords DVU.! 1)) 8 .|. fromIntegral (awords DVU.! 0)

readIndexByte
  :: Z80PC
  -> Z80system sysType
  -> (Int8, Z80PC, Z80system sysType)
readIndexByte pc sys = (fromIntegral disp, pc', sys')
    where
      (pc', (disp, sys')) = sysIncPCAndRead pc sys

read16BitValue
  :: (AbstractAddr Z80addr -> Z80operand opndType)
  -> OpLocation sysType
  -> (Z80operand opndType, Z80PC, Z80system sysType)
read16BitValue opndCTor (pc, sys) = (opndCTor . mkAbstractAddr $ addr, pc + 3, sys')
  where
    (addr, sys') = z80getAddr sys (pc + 1)

read8BitValue
  :: (Z80byte -> Z80operand opndType)
  -> OpLocation sysType
  -> (Z80operand opndType, Z80PC, Z80system sysType)
read8BitValue opndCTor (pc, sys) = (opndCTor immval, 1+ newpc, sys')
  where
    (newpc, (immval, sys')) = sysIncPCAndRead pc sys

mkRegPairSP
  :: Z80byte
  -- ^ Register code
  -> RegPairSP
  -- ^ Resulting register pair
mkRegPairSP 0 = RPair16 BC
mkRegPairSP 1 = RPair16 DE
mkRegPairSP 2 = RPair16 HL
mkRegPairSP 3 = SP
mkRegPairSP x = error ("pairSP: invalid 16-bit register code " ++ show x)

-- | Convert 16-bit register pair/SP index to a 'RegPairSP' operand
mkRegPairAF
  :: Z80byte                               -- ^ Register pair index
  -> RegPairAF                             -- ^ Resulting register pair
mkRegPairAF 0 = AFPair16 BC
mkRegPairAF 1 = AFPair16 DE
mkRegPairAF 2 = AFPair16 HL
mkRegPairAF 3 = AF
mkRegPairAF x = error ("mkRegPairAF: invalid 16-bit register code " ++ show x)

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

completeInstruction
  :: Z80instruction
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)
completeInstruction insn (pc, sys) = (insn, 1+ pc, sys)

resultLoad
  :: IndexRegSubs
  -> Z80byte
  -> OpcComponents
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)

resultLoad xform opcGroup opcParts (pc, sys) =
  let (opnd, pc', _sys') = decodeOperands xform opcGroup opcParts (pc, sys)
  in  (LD opnd, pc', sys)

resultLoad16
  :: IndexRegSubs
  -> Z80byte
  -> OpcComponents
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)

resultLoad16 xform opcGroup opcParts (pc, sys) =
  let (opnd, pc', _sys') = decodeOperands xform opcGroup opcParts (pc, sys)
  in  (LD16 opnd, pc', sys)

resultALU16
  :: (Z80operand Z80OpndALU16 -> Z80instruction)
  -> IndexRegSubs
  -> Z80byte
  -> OpcComponents
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)

resultALU16 alu16CTor xform opcGroup opcParts (pc, sys) =
  let (opnd, pc', _sys') = decodeOperands xform opcGroup opcParts (pc, sys)
  in  (alu16CTor opnd, 1+ pc', sys)

resultIncDec
  :: (Z80operand Z80OpndInc -> Z80instruction)
  -> IndexRegSubs
  -> Z80byte
  -> OpcComponents
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)

resultIncDec insnCTor xform opcGroup opcParts opLocn@(_pc, sys) =
  let (opnd, pc', _sys') = decodeOperands xform opcGroup opcParts opLocn
  in  (insnCTor opnd, pc', sys)

-- | Compute signed, relative displacement address' absolute address
displacementInstruction
  :: (AbstractAddr Z80addr -> Z80instruction)
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)

displacementInstruction jumpIns (pc, sys) =
  let (_, (disp, _sys'))  = sysIncPCAndRead pc sys
      destAddr            = fromIntegral (pc + signExtend disp + 2)
  in  (jumpIns . mkAbstractAddr $ destAddr, pc + 2, sys)

-- | Fetch address, insert into an instruction
mkAbsAddrInsn
  :: (AbstractAddr Z80addr -> Z80instruction)
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)
mkAbsAddrInsn insnCTor (pc, sys) = (insnCTor . mkAbstractAddr $ addr, pc + 3, sys')
  where
    (addr, sys') = z80getAddr sys (pc + 1)

-- | The SET, RESet, BIT instructions and rotation operations.
bitopsDecode
  :: Z80byte
  -- ^ Opcode group
  -> OpcComponents
  -- ^ '(z, y, p, q) opcode components
  -> IndexRegSubs
  -- ^ Index register substitutions
  -> OpLocation sysType
  -- ^ Bit/rotate operation opcode components
  -> (Z80instruction, Z80PC, Z80system sysType)
  -- ^ The result
bitopsDecode _opcGroup _opcParts xform (pc, sys) =
  let (pc', (nextOpc, sys')) = sysIncPCAndRead pc sys
      (z, y, _, _) = opcodeComponents nextOpc
      dest = fromMaybe (error $ "reg8: Invalid register index: " ++ show z)
                       (IntMap.lookup (fromIntegral z) reg8NumToReg)
      insnCTor =
        case opcodeGroup nextOpc of
          0          -> rotOps ! fromIntegral y
          1          -> BIT y
          2          -> RES y
          3          -> SET y
          _otherwise -> undefined
      (opnd, pc'', sys'') = indexSubstitute xform dest (pc', sys')
  in  (insnCTor opnd, 1+ pc'', sys'')

-- | Undocumented rotate/shift/bit operations using IX and IY
undocBitOpsDecode
  :: Z80byte
  -- ^ Original prefix, 0xdd or 0xfd
  -> OpLocation sysType
  -- ^ Bit/rotate operation opcode components
  -> (Z80instruction, Z80PC, Z80system sysType)
  -- ^ The result
undocBitOpsDecode prefixOpc (pc, sys) =
  let (pcStep1, (disp, sys')) = sysIncPCAndRead pc sys
      (pcStep2, (opc, sys'')) = sysIncPCAndRead pcStep1 sys'
      idxReg8Ctor 0xdd = IXindirect . fromIntegral
      idxReg8Ctor 0xfd = IYindirect . fromIntegral
      idxReg8Ctor _    = undefined
      x = opcodeGroup opc
      (z, y, _p, _q) = opcodeComponents opc
      theReg = reg8NumToReg ! fromIntegral z
      instruction
        | theReg == HLindirect
        = case x of
            0          -> (rotOps ! fromIntegral y) (idxReg8Ctor prefixOpc disp)
            1          -> BIT y (idxReg8Ctor prefixOpc disp)
            2          -> RES y (idxReg8Ctor prefixOpc disp)
            3          -> SET y (idxReg8Ctor prefixOpc disp)
            _otherwise -> undefined
        | otherwise
        = case x of
            0          -> (undocRotOps ! fromIntegral y) (idxReg8Ctor prefixOpc disp) theReg
            1          -> BIT y (idxReg8Ctor prefixOpc disp)
            2          -> RESidx y (idxReg8Ctor prefixOpc disp) theReg
            3          -> SETidx y (idxReg8Ctor prefixOpc disp) theReg
            _otherwise -> undefined
  in  (instruction, 1+ pcStep2, sys'')

undocRotOps
  :: IntMap (Z80reg8 -> Z80reg8 -> Z80instruction)
undocRotOps = IntMap.fromList
  [ (0, RLCidx)
  , (1, RRCidx)
  , (2, RLidx)
  , (3, RRidx)
  , (4, SLAidx)
  , (5, SRAidx)
  , (6, SLLidx)
  , (7, SRLidx)
  ]

portOpDecode
  :: (Z80operand Z80OpndIO -> Z80instruction)
  -> Z80byte
  -> OpcComponents
  -> IndexRegSubs
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)
portOpDecode insn opcGroup opcParts xform opLocn =
  let (opnd, pc', sys') = decodeOperands xform opcGroup opcParts opLocn
  in  (insn opnd, pc', sys')

-- | Decode 'ED'-prefixed instructions
edPrefixDecode
  :: Z80byte
  -- ^ Opcode group
  -> OpcComponents
  -- ^ '(z, y, p, q) opcode components
  -> IndexRegSubs
  -- ^ Index register substitutionsresultLoad
  -> OpLocation sysType
  -- ^ Bit/rotate operation opcode components
  -> (Z80instruction, Z80PC, Z80system sysType)
  -- ^ The result

edPrefixDecode  _opcGroup _opcParts xform (pc, sys) =
  let (pc', (nextOpc, sys')) = sysIncPCAndRead pc sys
      invalidOpcodes = [0xed, nextOpc]
      nextOpcGroup = opcodeGroup nextOpc
      nextOpcParts = opcodeComponents nextOpc
  in
    case nextOpcGroup of
      1 -> edGroup1Decode invalidOpcodes nextOpcGroup nextOpcParts xform (pc', sys')
      2 -> edGroup2Decode invalidOpcodes nextOpcGroup nextOpcParts xform (pc', sys')
      _ -> completeInstruction (Z80undef (UndefInsn invalidOpcodes)) (pc', sys')

edGroup1Decode, edGroup2Decode, blockInstruction
  :: [Z80byte]
  -> Z80byte
  -> OpcComponents
  -> IndexRegSubs
  -> OpLocation sysType
  -> (Z80instruction, Z80PC, Z80system sysType)

edGroup1Decode _ops  opcGroup  opcParts@(0, _, _, _)  xform opLocn = portOpDecode IN  opcGroup opcParts xform opLocn
edGroup1Decode _ops  opcGroup  opcParts@(1, _, _, _)  xform opLocn = portOpDecode OUT opcGroup opcParts xform opLocn
edGroup1Decode _ops  opcGroup  opcParts@(2, _, _, 0)  xform opLocn = resultALU16 SBC16 xform opcGroup opcParts opLocn
edGroup1Decode _ops  opcGroup  opcParts@(2, _, _, 1)  xform opLocn = resultALU16 ADC16 xform opcGroup opcParts opLocn
edGroup1Decode  ops _opcGroup           (2, _, _, _) _xform opLocn = completeInstruction (Z80undef (UndefInsn ops)) opLocn
edGroup1Decode _ops  opcGroup  opcParts@(3, _, _, _)  xform opLocn = resultLoad16 xform opcGroup opcParts opLocn
edGroup1Decode _ops _opcGroup           (4, _, _, _) _xform opLocn = completeInstruction NEG opLocn
edGroup1Decode _ops _opcGroup           (5, _, _, 0) _xform opLocn = completeInstruction RETN opLocn
edGroup1Decode _ops _opcGroup           (5, _, _, 1) _xform opLocn = completeInstruction RETI opLocn
edGroup1Decode _ops _opcGroup           (6, y, _, _) _xform opLocn = completeInstruction (IM (interruptMode ! fromIntegral y)) opLocn
edGroup1Decode _ops _opcGroup           (7, 0, _, _) _xform opLocn = completeInstruction (LD FromAtoI) opLocn
edGroup1Decode _ops _opcGroup           (7, 1, _, _) _xform opLocn = completeInstruction (LD FromAtoR) opLocn
edGroup1Decode _ops _opcGroup           (7, 2, _, _) _xform opLocn = completeInstruction (LD FromItoA) opLocn
edGroup1Decode _ops _opcGroup           (7, 3, _, _) _xform opLocn = completeInstruction (LD FromRtoA) opLocn
edGroup1Decode _ops _opcGroup           (7, 4, _, _) _xform opLocn = completeInstruction RRD opLocn
edGroup1Decode _ops _opcGroup           (7, 5, _, _) _xform opLocn = completeInstruction RLD opLocn
edGroup1Decode  ops _opcGroup _opcParts _xform opLocn = completeInstruction (Z80undef (UndefInsn ops)) opLocn

edGroup2Decode  ops  opcGroup  opcParts@(_, 4, _, _)  xform opLocn = blockInstruction ops opcGroup opcParts xform opLocn
edGroup2Decode  ops  opcGroup  opcParts@(_, 5, _, _)  xform opLocn = blockInstruction ops opcGroup opcParts xform opLocn
edGroup2Decode  ops  opcGroup  opcParts@(_, 6, _, _)  xform opLocn = blockInstruction ops opcGroup opcParts xform opLocn
edGroup2Decode  ops  opcGroup  opcParts@(_, 7, _, _)  xform opLocn = blockInstruction ops opcGroup opcParts xform opLocn
edGroup2Decode  ops _opcGroup _opcParts _xform opLocn = completeInstruction (Z80undef (UndefInsn ops)) opLocn

blockInstruction  ops _opcGroup        (z, y, _, _) _xform opLocn =
  let insn =
        if z <= 3 && y >=4
        then (blockOps ! fromIntegral y) ! fromIntegral z
        else Z80undef (UndefInsn ops)
  in completeInstruction insn opLocn

-- | Convert condition code
condCode
  :: Z80byte
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

aluOp
  :: Z80byte
  -> (Z80operand Z80OpndALU -> Z80instruction)
aluOp 0 = ADD
aluOp 1 = ADC
aluOp 2 = SUB
aluOp 3 = SBC
aluOp 4 = AND
aluOp 5 = XOR
aluOp 6 = OR
aluOp 7 = CP
aluOp x = error ("aluOp: Invalid ALU operation index " ++ show x)

rotOps
  :: IntMap (Z80reg8 -> Z80instruction)
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

-- | Convert embedded interrupt mode to actual interrupt mode
interruptMode :: IntMap Z80byte
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

-- | Block/compare/input/output increment-decrement lookup table
blockOps :: IntMap (IntMap Z80instruction)
blockOps = IntMap.fromList
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
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

group0decode, group1decode, group2decode, group3decode
  :: [Z80byte]
  -> Z80byte
  -> OpcComponents
  -- ^ Current opcode
  -> IndexRegSubs
  -> OpLocation sysType
  -- ^ State
  -> (Z80instruction, Z80PC, Z80system sysType)
  -- ^ (Instruction, New disassembly state)
group0decode _ops _opcGroup (0, 0, _, _)          _xform  opLocn = completeInstruction NOP opLocn
group0decode _ops _opcGroup (0, 1, _, _)          _xform  opLocn = completeInstruction (EXC AFAF') opLocn
group0decode _ops _opcGroup (0, 2, _, _)          _xform  opLocn = displacementInstruction DJNZ opLocn
group0decode _ops _opcGroup (0, 3, _, _)          _xform  opLocn = displacementInstruction JR opLocn
group0decode _ops _opcGroup (0, y, _, _)          _xform  opLocn = displacementInstruction (JRCC $ condCode (y - 4)) opLocn
group0decode _ops  opcGroup opcParts@(1, _, _, 0)  xform  opLocn = resultLoad xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(1, _, _, 1)  xform  opLocn = resultALU16 ADD16 xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(2, _, 2, 0)  xform  opLocn = resultLoad16 xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(2, _, 2, 1)  xform  opLocn = resultLoad16 xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(2, _, _, _)  xform  opLocn = resultLoad xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(3, _, _, 0)  xform  opLocn = resultIncDec INC xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(3, _, _, 1)  xform  opLocn = resultIncDec DEC xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(4, _, _, _)  xform  opLocn = resultIncDec INC xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(5, _, _, _)  xform  opLocn = resultIncDec DEC xform opcGroup opcParts opLocn
group0decode _ops  opcGroup opcParts@(6, _, _, _)  xform  opLocn = resultLoad xform opcGroup opcParts opLocn
group0decode _ops _opcGroup (7, y, _, _)          _xform  opLocn = completeInstruction (accumOps ! fromIntegral y) opLocn
group0decode  ops _opcGroup _opcParts             _xform  opLocn = completeInstruction (Z80undef (UndefInsn ops)) opLocn

-- LD HL, HL -> HALT
-- Everything else is a LD reg8, something
group1decode _ops _opcGroup (6, 6, _, _) _xform opLocn = completeInstruction HALT opLocn
group1decode  _ops opcGroup opcParts      xform opLocn = resultLoad xform opcGroup opcParts opLocn

group2decode _ops opcGroup opcParts@(_, y, _, _) xform (pc, sys) =
  let insnCTor = aluOp y
      (opnd, pc', _sys') = decodeOperands xform opcGroup opcParts (pc, sys)
  in  (insnCTor opnd, pc', sys)

group3decode _ops _opcGroup (0, y, _, _) _xform  opLocn = completeInstruction (RETCC $ condCode y) opLocn

group3decode _ops _opcGroup (1, _, p, 0)  xform  opLocn =
  let (opnd, pc', sys') = indexSubstitute xform (mkRegPairAF p) opLocn
  in  (POP opnd, 1+ pc', sys')
group3decode _ops _opcGroup (1, _, 0, 1) _xform  opLocn = completeInstruction RET opLocn
group3decode _ops _opcGroup (1, _, 1, 1) _xform  opLocn = completeInstruction (EXC Primes) opLocn
group3decode _ops _opcGroup (1, _, 2, 1)  xform  opLocn = indexSubstitute xform  `applyTransformer` completeInstruction JPHL opLocn
group3decode _ops _opcGroup (1, _, 3, 1)  xform  opLocn = indexSubstitute xform  `applyTransformer` completeInstruction LDSPHL opLocn

group3decode _ops _opcGroup (2, y, _, _) _xform  opLocn = mkAbsAddrInsn (JPCC (condCode y)) opLocn

group3decode _ops _opcGroup          (3, 0, _, _) _xform  opLocn = mkAbsAddrInsn JP opLocn
group3decode _ops opcGroup  opcParts@(3, 1, _, _) xform  opLocn = bitopsDecode opcGroup opcParts xform opLocn
group3decode _ops opcGroup  opcParts@(3, 2, _, _) xform  opLocn = portOpDecode OUT opcGroup opcParts xform opLocn
group3decode _ops opcGroup  opcParts@(3, 3, _, _) xform  opLocn = portOpDecode IN opcGroup opcParts xform opLocn
group3decode _ops _opcGroup  (3, 4, _, _) xform  opLocn =
  let (opnd, pc', sys') = indexSubstitute xform SPHL opLocn
  in  (EXC opnd, 1+ pc', sys')
group3decode _ops _opcGroup  (3, 5, _, _) _xform  opLocn = completeInstruction (EXC DEHL) opLocn
group3decode _ops _opcGroup  (3, 6, _, _) _xform  opLocn = completeInstruction DI opLocn
group3decode _ops _opcGroup  (3, 7, _, _) _xform  opLocn = completeInstruction  EI opLocn
group3decode _ops _opcGroup (4, y, _, _) _xform  opLocn = mkAbsAddrInsn (CALLCC (condCode y)) opLocn
group3decode _ops _opcGroup (5, _, p, 0)  xform  opLocn =
  let (opnd, pc', sys') = indexSubstitute xform (mkRegPairAF p) opLocn
  in  (PUSH opnd, 1+ pc', sys')
group3decode _ops _opcGroup (5, _, 0, 1) _xform  opLocn = mkAbsAddrInsn CALL opLocn
group3decode  ops _opcGroup (5, _, 1, 1) _xform  opLocn  = completeInstruction (Z80undef (UndefInsn ops)) opLocn
group3decode _ops opcGroup opcParts@(5, _, 2, 1)  xform  opLocn  = edPrefixDecode opcGroup opcParts xform opLocn
group3decode _ops opcGroup opcParts@(6, y, _, _)   xform (pc, sys) =
  let (opnd, pc', _sys') = decodeOperands xform opcGroup opcParts (pc, sys)
  in  (aluOp y opnd, pc', sys)
group3decode _ops _opcGroup (7, y, _, _) _xform opLocn = completeInstruction (RST (y * 8)) opLocn
group3decode ops _opcGroup _opcParts              _xform opLocn = completeInstruction (Z80undef (UndefInsn ops)) opLocn

z80system :: Z80system Z80BaseSystem
z80system =  z80generic & processor . processorOps . idecode .~ zz_z80InsDecode

main :: IO ()
main = mapM_ doTestGroup z80InstData
  where
    doTestGroup tgrp = do
      putStrLn (groupDescription tgrp)
      mapM_ doInstTest (testCases tgrp)

    doInstTest :: InstTestCase -> IO ()
    doInstTest tdata = do
        if expectedInst == (inst' ^. decodedInsn)
        then z80AnalyticDisassemblyOutput stdout dstate' output
        else do
            putStrLn "---- fixme:"
            z80AnalyticDisassemblyOutput stdout dstate' output
            z80AnalyticDisassemblyOutput stdout check_dstate' check_output
            putStrLn "-----------"
            error "Undecoded instruction."
      where
        insnBytes      = DVU.fromList $ instBytes tdata
        testsys        = z80system
                          & memory .~ mkROMRegion testOrigin insnBytes mempty
        refsys         = z80generic
                          & memory .~ mkROMRegion testOrigin insnBytes mempty

        sAddr = testOrigin
        eAddr = testOrigin + fromIntegral (length $ instBytes tdata)

        dstate = mkZ80DisassemblyState testsys sAddr eAddr
        (output, dstate') = disassembler dstate

        check_dstate = mkZ80DisassemblyState refsys sAddr eAddr
        (check_output, check_dstate') = disassembler check_dstate

        expectedInst = instruction tdata

        (inst', _sys') = zz_z80InsDecode (PC testOrigin) testsys
