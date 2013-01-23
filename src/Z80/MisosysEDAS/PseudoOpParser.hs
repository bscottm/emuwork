{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Pseudo-operation parser for the Misosys EDAS-compatible assembler
--
-- (Historical note: This is a refactor of the 'Parser.hs' source because the function was getting large.)
module Z80.MisosysEDAS.PseudoOpParser
 ( asmPseudo
 ) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Text()                 -- ghci 7.6.1 needs these imported instances, e.g., (Stream T.Text Identity Char)
import qualified Data.Text as T
import qualified Data.Char as C

import Z80.MisosysEDAS.Types
import Z80.MisosysEDAS.ParserUtils
import Z80.MisosysEDAS.ExprParser

-- | Parse EDAS pseudo operations, such as "EQU", "DEFS", "ORG", etc.
asmPseudo :: EDASParser AsmOp
asmPseudo =
  liftM Pseudo $ getPosition
                 >>= (\srcloc -> 
                       ( charIC 'd'
                         >> ( ( stringIC "ate" >> return AsmDate )    -- date
                              <|> ( charIC 'b' >> parseDefB )         -- db (define byte sequence)
                              <|> ( charIC 'c'                        -- dc (define replicated byte constant)
                                    >> whiteSpace
                                    >> asmExpr
                                    >>= (\rept -> optional whiteSpace
                                                  >> char ','
                                                  >> optional whiteSpace
                                                  >> asmExpr
                                                  >>= (\fill -> return $ DefC rept fill)
                                        )
                                  )
                              <|> ( stringIC "ef"
                                    >> ( ( charIC 'b' >> parseDefB )          -- defb (aka 'db')
                                         <|> ( charIC 'l' >> parseDefL )      -- defl (define re-assignable label)
                                         <|> ( charIC 'm' >> parseDefB )      -- defm (aka 'db')
                                         <|> ( charIC 's' >> parseDefS )      -- defs (aka 'ds')
                                         <|> ( charIC 'w' >> parseDefW )      -- defw
                                       )
                                  )
                              <|> ( charIC 'm' >> parseDefB )         -- dm (same as 'db')
                              <|> ( charIC 's'
                                    >> ( try ( ( notFollowedBy letter
                                                 >> parseDefS         -- ds (define space, same as 'dc', but the constant is 0)
                                               ) <?> "constant or expression following DS"
                                             )
                                         <|> ( ( stringIC "ym"        -- dsym (dump symbol, emits symbol's name as byte sequence)
                                                 >> whiteSpace
                                                 >> liftM DSym readLabel
                                               ) <?> "label to follow DSYM"
                                             )
                                       )
                                  )
                              <|> ( charIC 'w' >> parseDefW )         -- dw (define words)
                              <|> ( charIC 'x'                        -- dx (define expression: emit 16-bit expression value)
                                    >> whiteSpace
                                    >> liftM DExp asmExpr
                                  )
                            )
                       )
                   <|> ( charIC 'e'
                           >> ( ( stringIC "qu"                     -- equ (symbol equation)
                                  >> pseudoWithExpr Equate
                                )
                                <|> try ( stringIC "nd"             -- end
                                          >> whiteSpace
                                          >> liftM (EndAsm srcloc) (optionMaybe asmExpr)
                                        )
                                <|> try ( stringIC "ntry"           -- entry
                                          >> pseudoWithExpr (Entry srcloc)
                                        )
                              )
                       )
                   <|> ( stringIC "lorg"
                         >> pseudoWithExpr LoadOrg
                       )
                   <|> ( stringIC "org"
                         >> pseudoWithExpr Origin
                       )
                   <|> ( stringIC "time" >> return AsmTime )
                 )
  where
    pseudoWithExpr pseudo = whiteSpace
                            >> liftM pseudo asmExpr

    -- DB/DEFB/DEFM/DM: a list of strings, single characters, expressions or bytes (which are constant expressions)
    -- Note that range checking is done by the assembler.
    parseDefB = whiteSpace
                >> ( liftM DefB $ sepBy1 defBArgs ( char ',' >> optional whiteSpace ) )

    -- According to the EDAS manual, strings in DEFB/DB lists are 'c' (single quoted characters) or a sequence
    -- of two or more characters that can include a double-single quote ("''") interpreted as, well, a single
    -- quote in the middle of a string.
    defBArgs = try (  -- Might read 'AsmChar', which could consume a single quote
                      liftM DBExpr asmExpr
                      <?> "expression in DEFB/DB argument list"
                   )
               <|> ( do { _  <- char '\''
                        ; c1 <- asciiExceptSQuote
                        ; c2 <- asciiExceptSQuote
                        ; s1 <- many ( try ( char '\''
                                             >> char '\''
                                             >> return '\''
                                           )
                                       <|> asciiExceptSQuote
                                     )
                        ; _ <- char '\''
                        ; return ((DBStr . T.pack) (c1:c2:s1))
                        }
                     <?> "string in DEFB/DB argument list"
                   )

    -- DS/DEFS parser
    parseDefS = whiteSpace
                >> liftM DefS asmExpr

    -- DW/DEFW parser
    parseDefW = whiteSpace
                >> ( liftM DefW $ sepBy1 defWArgs ( char ',' >> optional whiteSpace ) )

    defWArgs = try ( liftM DWExpr asmExpr ) -- Might read 'AsmChar', which could consume a single quote
               <|> ( char '\''
                     >> asciiChar
                     >>= (\c1 -> asciiChar
                                 >>= (\c2 -> char '\'' >> (return $ DWChars c1 c2))
                         )
                   )

    -- DEFL parser
    parseDefL = whiteSpace
                >> liftM DefL asmExpr

-- | Parse any printable ASCII character, except for a single quote.
asciiExceptSQuote :: EDASParser Char
asciiExceptSQuote =
  let showC x           = [ '\'', x, '\'' ]
      testC x           = let x' = C.ord x
                          in  if x' /= 0x27 && (x' >= 0x20 && x' <= 0x7e) then 
                                Just x
                              else
                                Nothing
      nextPos pos x _xs = updatePosChar pos x
  in  tokenPrim showC nextPos testC
