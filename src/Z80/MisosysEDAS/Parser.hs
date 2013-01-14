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

#ifdef mingw32_HOST_OS
import Control.Lens hiding (value, walk, op)
#else
import Control.Lens hiding (value, walk)
#endif

import Text.Parsec
import Text.Parsec.Text()                 -- ghci 7.6.1 needs these imported instances, e.g., (Stream T.Text Identity Char)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as DVU

import Z80.MisosysEDAS.Types
import Z80.MisosysEDAS.ParserUtils
import Z80.MisosysEDAS.PseudoOpParser
import Z80.MisosysEDAS.MnemonicParser
import Z80.MisosysEDAS.ExprParser

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
initialAsmStmt = AsmStmt { _symLabel = Nothing
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
asmStatement =
  do { stmtLabel <- collectLabel
     ; try ( do { asmop <- collectOp stmtLabel
                ; cmnt  <- collectComment
                ; let ustate = (symLabel .~ stmtLabel) $ (asmOp .~ asmop) $ (comment .~ cmnt) $ initialAsmStmt
                ; {- trace (show ustate) $ return ustate -}
                ; return ustate
                }
           )
       <|> try (conditionalAsm stmtLabel)
       -- Add alternative here for macro invocation
     }

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
          -> EDASParser AsmOp
collectOp stmtLabel = skipLabelColon stmtLabel
                      >> option NoAsmOp ( asmMnemonic
                                          <|> asmPseudo
                                        ) <?> "Z80 instruction or pseudo-operation"

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

-- | Skip a statement label's optional @:@
skipLabelColon :: Maybe EDASLabel
               -> EDASParser ()
skipLabelColon (Just _label) = optional (char ':') >> optional whiteSpace
skipLabelColon Nothing       = return ()

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
  do { skipLabelColon stmtLabel
     ; _ <- stringIC "if"
     ; do { whiteSpace
          ; expr <- asmExpr
          ; cmnt <- collectComment
          ; let ustate = CondAsmEval { _symLabel     = stmtLabel
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
          ; putState ustate
          ; condStatements
          ; getState
          }
     }

-- | Parse and collect conditional statements. Note that this function relies on modifying the parser state, which is an
-- 'AsmStmt'. This saves the code from having to pass an 'AsmStmt' around explicitly.
condStatements :: EDASParser ()
condStatements = condStatements' []
  where
    condStatements' stmts =
      do { stmtLabel <- collectLabel
         ; skipLabelColon stmtLabel
         ; do { _    <- try ( stringIC "else" )
              ; cmnt <- collectComment
                -- Update the current state with the collected statements for the \"true\" part of the conditional
              ; modifyState (\ustate -> elseLabel .~ stmtLabel $
                                        elseComment .~ cmnt $
                                        stmtsTrue .~ (reverse stmts) $
                                        ustate
                            )
              ; elseStmts <- condElseStatements []
              ; modifyState (\ustate -> stmtsFalse .~ elseStmts $ ustate)
              ; return ()
              }
           <|> do { trueStmts <- condEndIf stmtLabel stmts
                  ; modifyState (\ustate -> stmtsTrue .~ trueStmts $ ustate)
                  ; return ()
                  }
           <|> do { ustate <- condAsmStatement stmtLabel
                  ; condStatements' (ustate:stmts)
                  }
         } <?> "'ELSE' or 'ENDIF' expected in 'IF' conditional assembly sequence"

    -- Common code for parsing "endif"
    condEndIf stmtLabel stmts =
      do { _    <- try ( stringIC "endif" )
         ; cmnt <- collectComment
         ; modifyState (\ustate -> endifLabel .~ stmtLabel $
                                   endifComment .~ cmnt $
                                   ustate
                       )
         ; return (reverse stmts)
         }

    -- Common code for collecting assembler statements until "else" or "endif" is encountered
    condAsmStatement stmtLabel = 
      do { asmop <- collectOp stmtLabel
         ; cmnt  <- collectComment
         ; return $ (symLabel .~ stmtLabel) $ (asmOp .~ asmop) $ (comment .~ cmnt) $ initialAsmStmt
         }

    -- Grab everything following the "else" statement. Note this is written for clarity, not for efficiency
    condElseStatements stmts =
      do { stmtLabel <- collectLabel
         ; skipLabelColon stmtLabel
         ; condEndIf stmtLabel stmts
           <|> do { ustate <- condAsmStatement stmtLabel
                  ; condElseStatements (ustate:stmts)
                  }
           <?> "'ENDIF' at the end of 'IF' conditional assembly sequence"
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
