{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Misosys EDAS assembler parser. There are two basic interfaces: 'edasParseFile' and 'edasParseSequence'. 'edasParseSequence'
-- parses a 'Data.Text' buffer.
module Z80.MisosysEDAS.Parser
  ( edasParseSequence
  , edasParseFile
  , asmStatement
  ) where

-- import Debug.Trace

import Control.Exception hiding (try)
import Control.Monad
import Control.Lens
import Text.Parsec
import Text.Parsec.Pos
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as DVU

import Z80.MisosysEDAS.Types
import Z80.MisosysEDAS.ParserUtils
import Z80.MisosysEDAS.PseudoOpParser
import Z80.MisosysEDAS.MnemonicParser
import Z80.MisosysEDAS.ExprParser
import Z80.MisosysEDAS.Assembler

-- | Parse an EDAS source file
edasParseFile :: FilePath
              -> IO (Either T.Text [AsmStmt])
edasParseFile path =
  if (not . null) path then
    (TIO.readFile path
     >>= (\edasInp -> return $ edasParseSequence path edasInp)) `catches` [ Handler (genericIOErrors "edasParseFile")
                                                                          -- Add more handlers here, as needed
                                                                          ]
  else
    return (Left "edasParseFile: Empty file name")

-- | Parse EDAS assembly input from a 'Data.Text' buffer.
edasParseSequence :: FilePath
                  -> T.Text
                  -> Either T.Text [AsmStmt]
edasParseSequence path inp =
  let manyEDASlines = do { result <- many asmStatement
                         ; return result
                         }
      properSrc     = if T.last inp /= '\n' then
                        inp `T.snoc` '\n'
                      else
                        inp
  in  case runParser manyEDASlines initialAsmStmt path properSrc of
        Left errmsg  -> Left $ ((T.pack . show) errmsg)
        Right result -> Right result

-- | Make an empty assembler statement
initialAsmStmt :: AsmStmt
initialAsmStmt = AsmStmt { _srcPos   = newPos "!!nosrc!!" 0 0
                         , _symLabel = Nothing
                         , _asmOp    = NoAsmOp
                         , _comment  = Nothing
                         , _stmtAddr = 0
                         , _bytes    = DVU.empty
                         }

-- | Parse an assembler statement, which has the form:
-- 
-- >label: op operands ; comment
--
-- All of the parts of the statement are optional.
asmStatement :: EDASParser AsmStmt
asmStatement = do { sourcePos <- getPosition
                  ; stmtLabel <- collectLabel
                  ; ustate <- parseOpAndComment stmtLabel
                  ; return ((srcPos .~ sourcePos) $ ustate)
                  }

-- | Common code for parsing what follows the statement label, see 'asmStatement'. The main reason why this is separated from
-- 'asmStatement' is that it is used in parsing conditionals, where the statement label has already been read and a sequence
-- of assembler statements are being collected. Insead of duplicating code, keep it all together in one function.
parseOpAndComment :: Maybe EDASLabel
                  -> EDASParser AsmStmt
parseOpAndComment stmtLabel =
  try ( do { ustate <- collectOp stmtLabel
           ; cmnt  <- collectComment
           ; let ustate' = (comment .~ cmnt) $ ustate
           ; {- trace (show ustate') $ return ustate -}
           ; return ustate'
           }
      )
  <|> conditionalAsm stmtLabel
  -- Add alternative here for macro invocation

-- | Collect the statement label. Note that either the statement label is present or there is whitespace to separate
-- and disambiguate the label from a mnemonic or pseudo-operation:
--
-- > label:      ld     a, 0ffh
--
-- versus:
--
-- >             ld      a, 0ffh
--
-- Without the whitespace, @ld@ could be misinterpreted or misparsed as the statement label.
collectLabel :: EDASParser (Maybe EDASLabel)
collectLabel = do { stmtLab <- readLabel
                  ; optional (char ':')
                  ; whiteSpace
                  ; return (Just stmtLab)
                  }
               <|> do { whiteSpace
                      ; return Nothing
                      }
               <?> "a statement label"

-- | Collect the assembler operation following the optional statement label parsed by 'collectLabel'. If the statement label is
-- present, it can be followed by an optional colon (@:@).
--
-- Valid assembler operations include: an instruction mnemonic and its arguments, or an assembler pseudo-operation.
collectOp :: Maybe EDASLabel
          -> EDASParser AsmStmt
collectOp stmtLabel = do { theOp <- option NoAsmOp ( asmMnemonic
                                                     <|> asmPseudo
                                                   ) <?> "Z80 instruction or pseudo-operation"
                         ; return ((symLabel .~ stmtLabel) $ (asmOp .~ theOp) $ initialAsmStmt)
                         }

-- | Collect the comment that optionally follows the assembler operation. Comments are start with a semicolon (@;@) and extend to
-- the end of the input line.
--
-- Note that collectComment reads up to the newline, but does not consume the newline.
collectComment :: EDASParser (Maybe Comment)
collectComment = optional whiteSpace
                 >> ( ( newline
                        >> return Nothing
                      ) <|> ( asmComment
                              >>= (\cmnt -> return (Just cmnt))
                            )
                    )

-- | Parse a comment, preserving its position in the input.
asmComment :: EDASParser Comment
asmComment =
  getPosition
  >>= (\curpos ->
        let srccol = sourceColumn curpos
        in  try (char ';')
            >> ( manyTill anyChar (try newline)
                 >>= (\cmnt -> return $ Comment srccol (T.pack cmnt))
               )
      )

-- | Conditional assembly parser, i.e., all of the @IF@ pseudo-operations. This parser is here, and not in
-- "Z80.MisosysEDAS.PseudoOpParser" because we need to parse and collect sequences of assembler statements.
-- Parsing statements means collecting labels, comments, etc.
conditionalAsm :: Maybe EDASLabel
               -> EDASParser AsmStmt
conditionalAsm stmtLabel =
  do {  sourcePos <- getPosition
     ; _ <- stringIC "if"
     ; try ( do { whiteSpace
                ; expr <- asmExpr
                ; cmnt <- collectComment
                ; condStatements CondAsmEval { _srcPos       = sourcePos
                                             , _symLabel     = stmtLabel
                                             , _evalExp      = expr
                                             , _comment      = cmnt
                                             , _stmtsTrue    = []
                                             , _elseLabel    = Nothing
                                             , _elseComment  = Nothing
                                             , _stmtsFalse   = []
                                             , _endifLabel   = Nothing
                                             , _endifComment = Nothing
                                             , _condResult   = True         -- Initial value doesn't really matter at this point
                                             }
                }
            )
       <|> try ( do { _ <- stringIC "eq"
                    ; parseCondCmp sourcePos "eq" (==)                      -- IFEQ
                      <|> parseCondCmpStr sourcePos "eq" (==)               -- IFEQ$
                    }
               )
       <|> try ( do { _ <- stringIC "lt"
                    ; parseCondCmp sourcePos "lt" (<)                       -- IFLT
                      <|> parseCondCmpStr sourcePos "lt" (<)                -- IFLT$
                    }
               )
       <|> try ( do { _ <- stringIC "gt"
                    ; parseCondCmp sourcePos "gt" (>)                       -- IFGT
                      <|> parseCondCmpStr sourcePos "gt" (>)                -- IFGT$
                    }
               )
       <|> try ( do { _ <- stringIC "ne"
                    ; parseCondCmp sourcePos "ne" (/=)                      -- IFNE
                      <|> parseCondCmpStr sourcePos "ne" (/=)               -- IFNE$
                    }
               )
     }
  where
    parseCondCmp sourcePos cmpOpName predF =
      do { let ifCmpName = T.append "IF" $ T.toUpper (cmpOpName)
         ; whiteSpace
         ; lExp <- asmExpr
         ; optional whiteSpace
         ; _ <- ( charIC ','
                  <|> fail (T.unpack (T.append "Comma expected between left and right sides of " ifCmpName))
                )
         ; optional whiteSpace
         ; rExp <- asmExpr
         ; cmnt <- collectComment
         ; condStatements CondCmp { _srcPos       = sourcePos
                                  , _symLabel     = stmtLabel
                                  , _cmpName      = cmpOpName
                                  , _leftExp      = lExp
                                  , _rightExp     = rExp
                                  , _condF        = (\ctx -> liftM2 predF (evalAsmExpr lExp ctx)
                                                                          (evalAsmExpr rExp ctx)
                                                    )
                                  , _comment      = cmnt
                                  , _stmtsTrue    = []
                                  , _elseLabel    = Nothing
                                  , _elseComment  = Nothing
                                  , _stmtsFalse   = []
                                  , _endifLabel   = Nothing
                                  , _endifComment = Nothing
                                  , _condResult   = True         -- Don't care at this point, just initialization
                                  }
         }

    parseCondCmpStr sourcePos cmpOpName predF =
      do { _ <- char '$'
         ; let ifCmpName = T.append "IF" $ T.toUpper (cmpOpName)
         ; whiteSpace
         ; lExp <- manyTill anyChar (char ',')
         ; _ <- ( charIC ','
                  <|> fail (T.unpack (T.append "Comma expected between left and right sides of " ifCmpName))
                )
         ; rExp <- manyTill anyChar (try $ oneOf [' ', '\t', '\f', '\v', ';', '\n'])
         ; cmnt <- collectComment
         ; condStatements CondCmpStr { _srcPos       = sourcePos
                                     , _symLabel     = stmtLabel
                                     , _cmpName      = cmpOpName
                                     , _leftStr      = T.pack lExp
                                     , _rightStr     = T.pack rExp
                                     -- FIXME: May need to perform variable expansions on lExp and rExp
                                     , _strcmpF      = (\_ctx -> Right $ lExp `predF` rExp
                                                       )
                                     , _comment      = cmnt
                                     , _stmtsTrue    = []
                                     , _elseLabel    = Nothing
                                     , _elseComment  = Nothing
                                     , _stmtsFalse   = []
                                     , _endifLabel   = Nothing
                                     , _endifComment = Nothing
                                     , _condResult   = True
                                     }
         }

-- | Parse and collect conditional statements. Note that this function relies on modifying the parser state, which is an
-- 'AsmStmt'. This saves the code from having to pass an 'AsmStmt' around explicitly.
condStatements :: AsmStmt
               -> EDASParser AsmStmt
condStatements ustate = condStatements' ustate []
  where
    condStatements' condState stmts =
      do { sourcePos <- getPosition
         ; stmtLabel <- collectLabel
         ; do { _    <- try ( stringIC "else" )
              ; cmnt <- collectComment
                -- Update the current state with the collected statements for the \"true\" part of the conditional
              ; let condState1 = elseLabel .~ stmtLabel $
                                 elseComment .~ cmnt $
                                 stmtsTrue .~ (reverse stmts) $
                                 condState
              ; (condState2, elseStmts) <- condElseStatements condState1 []
              ; return (stmtsFalse .~ elseStmts $ condState2)
              }
           <|> do { (condState', trueStmts) <- condEndIf condState stmtLabel stmts
                  ; let condState'' = stmtsTrue .~ trueStmts $ condState'
                  ; return condState''
                  }
           <|> do { stmt <- parseOpAndComment stmtLabel
                  ; let stmt' = (srcPos .~ sourcePos) $ stmt
                  ; condStatements' condState (stmt':stmts)
                  }
         }
      <|> fail "Unterminated 'IF' condition, looking for 'ELSE' or 'ENDIF'"

    -- Common code for parsing "endif"
    condEndIf condState stmtLabel stmts =
      do { _    <- try ( stringIC "endif" )
         ; cmnt <- collectComment
         ; let condState' = endifLabel .~ stmtLabel $
                            endifComment .~ cmnt $
                            condState
         ; return (condState', reverse stmts)
         }

    -- Grab everything following the "else" statement. Note this is written for clarity, not for efficiency
    condElseStatements condState stmts =
      do { sourcePos <- getPosition
         ; stmtLabel <- collectLabel
         ; condEndIf condState stmtLabel stmts
           <|> do { stmt <- parseOpAndComment stmtLabel
                  ; let stmt' = (srcPos .~ sourcePos) $ stmt
                  ; condElseStatements condState (stmt':stmts)
                  }
          }

-- | Generic I/O exception handler. This outputs a message to 'stderr' of the form
--
-- @whence: exception@
genericIOErrors :: T.Text                       -- ^ An indication of where the error ocurred
                -> IOError                      -- ^ The IO exception
                -> IO (Either T.Text [AsmStmt]) -- ^ Result is always 'invalidVector'
genericIOErrors whence exc = return $ Left $ T.concat [ whence
                                                      , ": "
                                                      , ((T.pack . show) exc)
                                                      ]
