module Z80.Tests.Execute.Utils where

import           Control.Monad       (when)

import           Data.Bits
import           Data.List           as List
import           Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Lens.Micro.Platform
import           System.IO
import           Test.HUnit          (Assertion, assertBool)
import           Text.Printf

import           Machine
import           Z80
import Control.Monad.Trans.Writer (WriterT, tell)

-- | Compare two systems' registers: 'leftRegs' are the expected registers, 'rightRegs' are the actual values. If they
-- don't match, then the contents of both are printed to 'stdout'.
compareRegs
  :: Z80system sysType
  -> Z80system sysType
  -> Text
  -> IO Bool
compareRegs leftSys rightSys banner =
  do
    when (leftRegs /= rightRegs) $
      do
        hFlush stderr
        TIO.hPutStrLn stderr $ T.intercalate "\n"
          [ banner
          , "Expected:"
          , printRegs leftRegs
          , "Got:"
          , printRegs rightRegs
          ]
        hFlush stderr
    return (leftRegs == rightRegs)
  where
    leftRegs  = z80registers leftSys
    rightRegs = z80registers rightSys

-- | Compare a memory location with an expected byte value, print an error message if they don't match.
compareMem
  :: Z80addr
  -> Z80byte
  -> Z80system sysType
  -> Text
  -> IO Bool
compareMem addr expectedVal z80sys banner =
  do
    let ctnt = fst $ sysMRead addr z80sys
    when (ctnt /= expectedVal) $
      do
        hFlush stderr
        TIO.hPutStrLn stderr $ T.concat
          [ banner
          , T.pack (printf " // compareMem Expected: 0x%02x, got 0x%02x" expectedVal ctnt)
          ]
        hFlush stderr
    return (ctnt == expectedVal)

-- | Compare a memory location with an expected byte value, print an error message if they don't match.
compareMem16
  :: Z80addr
  -> Z80addr
  -> Z80system sysType
  -> Text
  -> WriterT [Text] IO Bool
compareMem16 addr expectedVal z80sys banner =
  do
    let memCtnt = sysMReadN addr 2 z80sys
        ctnt = make16bit . fst $ memCtnt
        msg  = T.pack (printf " // compareMem16 Expected: 0x%02x, got 0x%02x" expectedVal ctnt)
    if ctnt /= expectedVal
    then tell
     [ T.empty
     , T.concat [ banner, msg]
     , T.concat [ "memory: ", T.pack . show .fst $ memCtnt]
     , T.empty
     ]
      >> return False
    else return True


-- | Dump registers to stdout.
printRegs
  :: Z80registers
  -> T.Text
printRegs zregs =
  T.pack $ printf "A: 0x%02x BC: 0x%04x DE: 0x%04x HL: 0x%04x IX: 0x%04x IY: 0x%04x SP: 0x%04x"
    (zregs ^. z80accum)
    (val16 z80breg z80creg)
    (val16 z80dreg z80ereg)
    (val16 z80hreg z80lreg)
    (val16 z80ixh  z80ixl)
    (val16 z80iyh  z80iyl)
    (zregs ^. z80sp)
  where
    val16 hi lo = ((fromIntegral (zregs ^. hi) :: Z80addr) `shiftL` 8) .|. (fromIntegral (zregs ^. lo) :: Z80addr)


-- | Dump processor flags to stdout.
printFlags
  :: String
  -> Z80state
  -> IO ()
printFlags banner z80 =
  hPutStrLn stderr $ printf "%s: S(%s) Z(%s) Y(%s) H(%s) X(%s) PO(%s) N(%s) C(%s)"
    banner valSign valZero valYFlag valHalfCarry valXFlag valParOv valNFlag valCarry
  where
    flagVal :: SimpleGetter Z80state Bool -> String
    flagVal getLens = if z80 ^. getLens then "T" else "F"
    valSign = flagVal flagSign
    valZero = flagVal flagZero
    valYFlag = flagVal flagYFlag
    valHalfCarry = flagVal flagHalfCarry
    valXFlag = flagVal flagXFlag
    valParOv = flagVal flagParOv
    valNFlag = flagVal flagNFlag
    valCarry = flagVal flagCarry

assertBoolMessages
    :: [Text]
    -> Assertion
assertBoolMessages msgs =
  do
    let result = List.filter (not . T.null) msgs
    assertBool (T.unpack $ T.intercalate "\n" result) (List.null result)
