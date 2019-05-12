module Z80.Execute.IncDec
  ( insIncDec
  )
where

import Data.Bits
import Data.Word
import Data.Int
import Lens.Micro.Platform

import Machine
import Z80.Execute.Utils
import Z80.InstructionSet
import Z80.Processor
import Z80.System

-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~
-- INC/DEC/INC16/DEC16 instructions:
-- ==~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~~

insIncDec
  ::Z80reg8
  -> (Z80word -> Z80word)
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
insIncDec A {-op nFlag sys-} = doIncDec z80accum z80accum {-op nFlag sys-}
insIncDec B {-op nFlag sys-} = doIncDec z80breg z80breg {-op nFlag sys-}
insIncDec C {-op nFlag sys-} = doIncDec z80creg z80creg {-op nFlag sys-}
insIncDec D {-op nFlag sys-} = doIncDec z80dreg z80dreg {-op nFlag sys-}
insIncDec E {-op nFlag sys-} = doIncDec z80ereg z80ereg {-op nFlag sys-}
insIncDec H {-op nFlag sys-} = doIncDec z80hreg z80hreg {-op nFlag sys-}
insIncDec L {-op nFlag sys-} = doIncDec z80lreg z80lreg {-op nFlag sys-}
insIncDec HLindirect {-op nFlag sys-} = indirectIncDec HL 0 {-op nFlag sys-}
insIncDec (IXindirect disp) {-op nFlag sys-} = indirectIncDec IX disp {-op nFlag sys-}
insIncDec (IYindirect disp) {-op nFlag sys-} = indirectIncDec IY disp {-op nFlag sys-}
insIncDec IXh {-op nFlag sys-} = doIncDec z80ixh z80ixh {-op nFlag sys-}
insIncDec IXl {-op nFlag sys-} = doIncDec z80ixl z80ixl {-op nFlag sys-}
insIncDec IYh {-op nFlag sys-} = doIncDec z80iyh z80iyh {-op nFlag sys-}
insIncDec IYl {-op nFlag sys-} = doIncDec z80iyl z80iyl {-op nFlag sys-}

doIncDec
  :: Getting Z80word Z80registers Z80word
  -> ASetter Z80registers Z80registers Z80word Z80word
  -> (Z80word -> Z80word)
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
doIncDec getLens setLens op nFlag sys =
  arithFlags oldval newval nFlag sys & processor . cpu . regs . setLens .~ newval
  where
    oldval = z80registers sys ^. getLens
    newval = op oldval

indirectIncDec
  :: Z80reg16
  -> Int8
  -> (Z80word -> Z80word)
  -> Bool
  -> Z80system sysType
  -> Z80system sysType
indirectIncDec HL _disp op nFlag sys = arithFlags oldval newval nFlag $ sysMWrite addr newval readSys
  where
    addr = reg16get HL sys
    (oldval, readSys) = sysMRead addr sys
    newval = op oldval
indirectIncDec BC _disp _op _nFlag _sys = undefined
indirectIncDec DE _disp _op _nFlag _sys = undefined
indirectIncDec reg disp op nFlag sys = arithFlags oldval newval nFlag $ sysMWrite addr newval readSys
  where
    addr = reg16get reg sys + signExtend disp
    (oldval, readSys) = sysMRead addr sys
    newval = op oldval
