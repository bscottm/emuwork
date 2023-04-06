module Z80.Arfmo where

import           Data.Generics.Uniplate.Operations
import           Data.Int                          (Int8)
import           Data.Vector.Unboxed               (Vector, fromList)

import           Machine
import           Machine.DisassemblerTypes()

import           Z80.Disassembler
import           Z80.InstructionSet
import           Z80.Processor

main :: IO ()
main = do
    wibbles
    wobbles
    wuffing
    happiness

wibbles :: IO ()
wibbles = print
    [ [ x | Reg8Reg8 _ x@HLindirect <- childrenBi (LD (Reg8Imm H 31)) ]
    , [ x | Reg8Reg8 _ x@HLindirect <- childrenBi (LD (Reg8Reg8 H HLindirect)) ]
    , childrenBi (LD (Reg8Reg8 H HLindirect))
    , universeBi (LD (Reg8Reg8 H HLindirect))
    , [ x | Reg8Reg8 x _ <- universeBi (LD (Reg8Reg8 H HLindirect)) ]
    , [ x | x :: Z80reg8 <- childrenBi (LD (Reg8Reg8 H HLindirect)) ]
    , [ x | x :: Z80reg8 <- universeBi (LD (Reg8Reg8 HLindirect H)) ]
    ]

wobbles :: IO ()
wobbles = print
    [ [ True | HLindirect <- childrenBi (LD (Reg8Imm H 31)) ]
    , [ True | HLindirect <- childrenBi (LD (Reg8Reg8 H HLindirect)) ]
    , [ True | HLindirect <- childrenBi (LD (Reg8Reg8 HLindirect D)) ]
    , [ HLindirect `elem` [ x | x :: Z80reg8 <- childrenBi (LD (Reg8Reg8 HLindirect B)) ] ]
    , [ HLindirect `elem` [ x | x :: Z80reg8 <- childrenBi (LD (Reg8Reg8 B HLindirect)) ] ]
    , [ HLindirect `elem` [ x | x :: Z80reg8 <- childrenBi (LD (Reg8Reg8 B L)) ] ]
    ]

quux :: IO ()
quux = print $ [ x | x :: [Z80instruction] <- universeBi <$> someData]

arfmo :: [Z80instruction]
arfmo = transformBi (ixalu 10) . 
        transformBi (ixreg8 6) <$> someData

someData :: [Z80instruction]
someData =
    [ LD (Reg8Imm H 31)
    , LD (Reg8Imm L 33)
    , LD (Reg8Imm HLindirect 37)
    , LD (Reg8Imm B 32)
    , LD (Reg8Reg8 B HLindirect)
    , LD (Reg8Reg8 H HLindirect)
    , LD (Reg8Reg8 L HLindirect)
    , INC (IncDecReg8 H)
    , INC (IncDecReg8 L)
    , INC (IncDecReg8 C)
    , AND (ALUreg8 C)
    , AND (ALUreg8 D)
    , AND (ALUreg8 H)
    , AND (ALUreg8 L)
    , AND (ALUreg8 HLindirect)
    ]

ixreg8 :: Int8 -> Z80operand Z80OpndLoad -> Z80operand Z80OpndLoad
ixreg8 _disp (Reg8Reg8 HLindirect IXh) = error "(HL), IXh is an invalid combination"
ixreg8 _disp (Reg8Reg8 HLindirect IXl) = error "(HL), IXl is an invalid combination"
ixreg8 _disp (Reg8Reg8 IXh HLindirect) = error "IXh, (HL) is an invalid combination"
ixreg8 _disp (Reg8Reg8 IXl HLindirect) = error "IYl, (HL) is an invalid combination"
ixreg8 disp (Reg8Reg8 dst HLindirect) = Reg8Reg8 dst (IXindirect disp)
ixreg8 disp (Reg8Reg8 HLindirect src) = Reg8Reg8 (IXindirect disp) src
-- ixreg8 _disp H = IXh
-- ixreg8 _disp L = IXl
-- ixreg8 disp HLindirect = IXindirect disp
ixreg8 disp (Reg8Imm dst imm) = Reg8Imm (xform disp dst) imm
ixreg8 _disp opnd = opnd

ixalu :: Int8 -> Z80operand Z80OpndALU -> Z80operand Z80OpndALU
ixalu disp (ALUreg8 reg) = ALUreg8 $ xform disp reg
ixalu _disp opnd = opnd

xform :: Int8 -> Z80reg8 -> Z80reg8
xform disp dst = case dst of
        H -> IXh
        L -> IXl
        HLindirect -> IXindirect disp
        _otherwise -> dst

wuffing :: IO ()
wuffing = print $ transformBi fixupSymbol callInst -- $ [ x | x :: AbstractAddr Z80addr <- childrenBi ]
    where
        callInst = CALL (mkAbstractAddr callDest)
        callDest = 0x4000 :: Z80addr
        fixupSymbol :: AbstractAddr Z80addr -> AbstractAddr Z80addr
        fixupSymbol addr = labelAbstractAddr addr "replace"

happiness :: IO ()
happiness = print $ [ x | x :: AbstractAddr Z80addr <- universeBi disElt ]
    where
        disElt :: Z80DisasmElt
        disElt = mkDisasmInsn (0x4f00 :: Z80addr)
                              (fromList [0x01, 0x02] :: Vector Z80byte)
                              (CALL (mkAbstractAddr callDest))
                              "A call instruction"
        callDest = 0x4000 :: Z80addr
