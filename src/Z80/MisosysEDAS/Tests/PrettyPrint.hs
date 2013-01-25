module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Z80.MisosysEDAS

main :: IO ()
main =
  do { asmResult <- edasAssemble $ edasParseSequence "prettyPrintTest" $
                      T.intercalate "\n" [ ";-- start of source --"
                                         , ""
                                         , "                                      ; Comment off to the right"
                                         , ""
                                         , "            ; Comment over the operation"
                                         , "C1          EQU   .not. 30"
                                         , "C2          EQU   .high. c1"
                                         , "C3          EQU   .low.c1"
                                         , "C4          EQU   .low. c1                ; Low component of C1"
                                         , "C5          EQU   C2.shl.8!c4.EQ.c1"
                                         , "C6          EQU   c2 .shl. 8!c4.eq.c1"
                                         , "C7          EQU   C2.shl.8 ! c4.EQ. C1"
                                         , "C8          EQU   c2<8!C4 .EQ. C1"
                                         , "C9          equ   c1<-8<8!c4 .eq. c1"
                                         , ""
                                         , "b1          equ   1001001b"
                                         , "b2          equ   -1001001b"
                                         , ""
                                         , "; -- start of data block -- "
                                         , "data:"
                                         , "            db    'This is a message', 0Dh, 00h"
                                         , "            dw    'AB', 'ab', 0FF0Fh"
                                         , ""
                                         , "sbyte:      if    0           ; Conditional block!"
                                         , "            db    'bad'       ; This should not get assembled"
                                         , "            else"
                                         , "            db    'Good!'"
                                         , "ebyte:      endif"
                                         , ""
                                         , "nbytes      equ   ebyte - sbyte"
                                         , ""
                                         , "sbyte1:     if    0           ; Conditional block!"
                                         , "            db    'bad'       ; This should not get assembled"
                                         , "mbyte1:     else"
                                         , "            db    'Good!'"
                                         , "ebyte1:     endif"
                                         , ""
                                         , "nbytes1     equ   mbyte1 - sbyte1"
                                         , "nbytes1a    equ   ebyte1 - sbyte1"
                                         , ""
                                         , "foo1:       if    1"
                                         , "            db    'Good'          ; This should output."
                                         , "            if    0               ; Nested conditional"
                                         , "            db    00H             ; But not this"
                                         , "            endif"
                                         , "            endif"
                                         , ""
                                         , "            ifgt  2,1"
                                         , "            endif"
                                         , ""
                                         , "            end   1234H"
                                         ]
      ; either (\msg -> TIO.putStrLn msg)
               (\(_ctx, stmts) -> do { {- putStrLn (show stmts) -}
                                     ; printAsmStmtsPretty stmts } )
               asmResult
      }
