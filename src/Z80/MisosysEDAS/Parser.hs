-- | Misosys EDAS assembler parser. There are two basic interfaces: 'edasParseOneLine' and 'edasParseSequence'. 'edasParseOneLine'
-- is intended to be used as a combinator, e.g., with parsing the analytic disassembly output.
module Z80.MisosysEDAS.Parser
  ( edasParseSequence
  , edasParseFile
  , asmStatement
  ) where

-- import Debug.Trace

import Control.Exception hiding (try)
import Control.Lens hiding (value, walk, op)
import Control.Monad
import Text.Parsec
import Text.Parsec.Pos
import Data.Text.Read
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as DVU

import Machine.EmulatedSystem
import Z80.InstructionSet
import Z80.MisosysEDAS.Types
import Z80.MisosysEDAS.Assembler

-- | Shorthand for the parser's type, leaving the "return" type free
type EDASParser retType = ParsecT T.Text AsmStmt Identity retType

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

-- | Parse multiple lines of EDAS assembly
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
  in  case runParser manyEDASlines mkEmptyAsmStmt path properSrc of
        Left errmsg  -> Left $ ((T.pack . show) errmsg)
        Right result -> Right result

-- | Make an empty assembler statement
mkEmptyAsmStmt :: AsmStmt
mkEmptyAsmStmt = AsmStmt { _symLabel = Nothing
                         , _asmOp    = Nothing
                         , _comment  = Nothing
                         , _stmtAddr = 0
                         , _bytes    = DVU.empty
                         }

-- Skip over whitespace. NB that the newline is important, so 'spaces' is not used here since it will skip over the
-- newline.
whiteSpace :: EDASParser ()
whiteSpace = skipMany (oneOf [' ', '\t', '\f', '\v'])

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

-- | Parse an assembler statement, which has the form "label: op operands ; comment". All of the parts of the statement are
-- optional.
asmStatement :: EDASParser AsmStmt
asmStatement =
  let updateStmtLab stmtLab asmStmt = asmStmt { _symLabel = stmtLab }
      updateAsmOp   op      asmStmt = asmStmt { _asmOp    = op      }
      updateComment cmnt    asmStmt = asmStmt { _comment  = cmnt    }

      collectLabel                  = do { stmtLab <- readLabel
                                         ; modifyState (updateStmtLab (Just stmtLab))
                                         ; return ()
                                         }
                                      <|> do { whiteSpace
                                             ; modifyState (updateStmtLab Nothing)
                                             ; return ()
                                             }
      collectInsnOrPseudo           = getState
                                      >>= (\theLabel ->
                                             case theLabel ^. symLabel of
                                                Just _label ->
                                                  -- ':' after a symbol might have an instruction following it.
                                                  do { optional (char ':')
                                                     ; optional whiteSpace
                                                     ; maybeAsmOrPseudo         <?> "Z80 instruction or pseudo-operation"
                                                     }
                                                  -- FIXME: What to do about equates that have no ':'?
                                                Nothing     ->
                                                  -- No label: expect an instruction or pseudo-op
                                                  maybeAsmOrPseudo              <?> "Z80 instruction or pseudo-operation"
                                          )
                                      >>= (modifyState . updateAsmOp)
      collectComment                = do { _ <- newline
                                         ; modifyState (updateComment Nothing )
                                         ; return ()
                                         }
                                      <|> do { cmnt <- asmComment
                                             ; modifyState (updateComment (Just cmnt))
                                             ; return ()
                                             }

      maybeAsmOrPseudo              = optionMaybe ( asmOpcode
                                                      <|> asmPseudo
                                                  )

  in  do { setState mkEmptyAsmStmt
         ; collectLabel
         ; collectInsnOrPseudo
         ; collectComment
         ; ustate <- getState
         ; {- trace (show ustate) $ -} return ustate
         }

-- | Parse a Z80 assembler opcode
asmOpcode :: EDASParser AsmOp
asmOpcode = 
  do { _ <- stringIC "ld"
     ; _ <- whiteSpace
     -- Treat accumulator loads a little differently from other 8-bit register loads, since A can be loaded
     -- indirectly from memory and other registers. Could have 'accumLoad' try and fail, but seems like that
     -- would be a slight performance hit that can easily be avoided with shared code.
     ; accumLoad
       <|> reg8Load
     }
  <|> do { _ <- stringIC "call"
         ; _ <- whiteSpace
         ; calldest <- asmExpr
         -- Evaluation context is curried into the last argument of absAddrEval
         ; returnInsn $ absAddrEval CALL calldest
         }
  <|> do { _ <- charIC 'r'
         ; do { _ <- stringIC "et"
              ; noEval RET
              }
           <|> do { _ <- stringIC "st"
                  ; optional whiteSpace
                  ; rstVecConst <- asmExpr
                  ; returnInsn (\ctx -> case evalAsmExprToWord8 ctx rstVecConst of
                                          Left stuff   -> Left stuff
                                          Right rstVec ->
                                            if rstVec `elem` [ 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38 ] then
                                              Right $ RST rstVec
                                            else
                                              Left "Invalid RST restart vector value"
                               )
                  } <?> "RET or RST instruction"
         }
  <|> do { _ <- stringIC "nop"
         ; noEval NOP
         }
  where
    -- Shorthand...
    returnInsn = return . Insn

    -- Simple case when an instruction doesn't have an expression to evaluate
    noEval z80insn = returnInsn (\_ctx -> Right z80insn)

    -- Simple case for 'AbsAddr'. Note that the 'AbsAddr' part of the data constructor is generally curried as the last
    -- argument to the 'Z80instruction'
    absAddrEval addrIns expr ctx =
      case evalAsmExpr ctx expr of
        Left stuff     -> Left stuff
        Right value    -> Right $ (addrIns . AbsAddr) value

    -- Simple case for 8-bit constants. This assumes that the 8-bit value is curried into the last argument of the 'insn'
    -- data constructor
    const8Eval insn expr ctx =
      case evalAsmExprToWord8 ctx expr of
        Left stuff     -> Left stuff
        Right value    -> Right $ insn value

    -- Accumulator loads
    accumLoad = try ( -- Accumulator is destination
                      do { _ <- charIC 'a'
                         ; notFollowedBy alphaNum
                         ; optional whiteSpace
                         ; _ <- char ','
                         ; optional whiteSpace
                         ; reg8LoadSources A
                         }
                    )
    -- 8-bit bit register/register loads
    reg8Load = do { dstReg <- parse8BitReg
                  ; optional whiteSpace
                  ; _ <- char ','
                  ; optional whiteSpace
                  ; reg8LoadSources dstReg
                  }

    -- Shared function between accumulator loads and 8-bit register loads
    reg8LoadSources dstReg = do { srcReg <- parse8BitReg
                                ; noEval (LD (Reg8Reg8 dstReg srcReg))
                                }
                             <|> do { expr <- asmExpr
                                    -- Context is curried into the last argument.
                                    ; returnInsn $ const8Eval (LD . (Reg8Imm dstReg)) expr
                                    }

    -- Parse an 8-bit register name. Valid register names come from the 'Z80.reg8Names' mapping or are an indirect
    -- register operand
    parse8BitReg = try ( do { reg8 <- choice (map (stringIC . T.unpack) (Map.keys reg8NameMap))
                            ; notFollowedBy alphaNum
                            ; return $ reg8NameToReg (T.pack reg8)
                            }
                         -- Handle the indirect register operands, (HL), (IX+d) and (IY+d)
                         <|> indirectReg8
                       )

    -- Indirect 8-bit register load/store
    indirectReg8 = between (char '(') (char ')')
                           ( do { optional whiteSpace
                                ; _ <- stringIC "hl"
                                ; optional whiteSpace
                                ; return HLindirect
                                }
                             <|> do { optional whiteSpace
                                    ; _ <- stringIC "ix"
                                    ; disp <- parseDisplacement
                                    ; optional whiteSpace
                                    ; return $ IXindirect disp
                                    }
                             <|> do { optional whiteSpace
                                    ; _ <- stringIC "iy"
                                    ; optional whiteSpace
                                    ; disp <- parseDisplacement
                                    ; return $ IYindirect disp
                                    }
                            )
    -- Parse an indexed displacement
    parseDisplacement = do { c <- constExpr True
                           ; case c of
                               Const disp -> if disp >= -128 && disp <= 127 then
                                               return disp
                                             else
                                               fail "index displacement out of bounds"
                               _otherwise  -> fail "index register displacement"
                            }

-- | Parse EDAS pseudo operations, such as "EQU", "DEFS", "ORG", etc.
asmPseudo :: EDASParser AsmOp
asmPseudo =
  (\pseudoOp -> return $ Pseudo pseudoOp)
  =<< ( pseudoWithExpr "equ" Equate
        <|> pseudoWithExpr "org" Origin
      )
  where
    pseudoWithExpr str pseudo = do { _ <- stringIC str
                                   ; whiteSpace
                                   ; liftM pseudo asmExpr
                                   }

-- | Assembler expression parsing. The expression must minimally have a primary expression (constant, variable name or unary
-- operator [".not.", ".high." or ".low."]) and may be optionally followed by binary operator expressions.
--
-- EDAS did not have a concept of operator precedence or parenthesized expressions, which is mimicked here as well.
asmExpr :: EDASParser EDASExpr
asmExpr =
  do { leftExpr <- primExpr
     ; option leftExpr (binOps leftExpr)
     }
  where
    -- Primary expressions: constants, variables/symbols/labels, unary operators
    primExpr = constExpr False
                 <|> do { srcpos <- getPosition
                        ; liftM (Var srcpos) readLabel
                        }
                 <|> ( unaryOp "not" OnesCpl
                       <|> unaryOp "high" HighByte
                       <|> unaryOp "low" LowByte
                     )

    -- Thankfully, EDAS makes parsing unary operations easy by putting them between '.'s:
    unaryOp opName opCtor = try ( do { _ <- between (char '.') (char '.') (stringIC opName)
                                     ; optional whiteSpace
                                     ; liftM opCtor primExpr
                                     }
                                )

    binOps leftExpr = do { optional whiteSpace
                         ; binOp leftExpr '+' Add
                             <|> binOp leftExpr '-' Sub
                             <|> binOp leftExpr '*' Mul
                             <|> binOp leftExpr '/' Div
                             <|> binOp leftExpr '<' Shift
                             <|> binOp leftExpr '&' LogAnd
                             <|> binOp leftExpr '!' LogOr
                             <|> try ( binOpSI leftExpr ".and." LogAnd )
                             <|> try ( binOpSI leftExpr ".or." LogOr )
                             <|> try ( binOpSI leftExpr ".mod." Mod )
                             <|> try ( binOpSI leftExpr ".xor." LogXor )
                             <|> try ( binOpSI leftExpr ".eq." LogEQ )
                             <|> try ( binOpSI leftExpr ".ge." LogGE )
                             <|> try ( binOpSI leftExpr ".gt." LogGT )
                             <|> try ( binOpSI leftExpr ".le." LogLE )
                             <|> try ( binOpSI leftExpr ".lt." LogLT )
                             <|> try ( binOpSI leftExpr ".shl." ShiftL )
                             <|> try ( binOpSI leftExpr ".shr." ShiftR )
                         }

    binOp leftExpr c op = do { _ <- char c
                             ; optional whiteSpace
                             ; rightExpr <- primExpr
                             ; let term = leftExpr `op` rightExpr
                             ; option term (binOps term)
                             }

    binOpSI leftExpr str op = do { _  <- stringIC str
                                 ; optional whiteSpace
                                 ; rightExpr <- primExpr
                                 ; let term = leftExpr `op` rightExpr
                                 ; option term (binOps term)
                                 }

-- Constant expression: Optional sign, digits and base
constExpr :: Bool                               -- ^ Sign required, 'True' when parsing IX, IY displacements
          -> EDASParser EDASExpr                -- ^ Result is an 'EDASExpr'
constExpr signRequired = do { sign <- if signRequired then
                                        signChars
                                      else
                                        option '+' signChars
                            ; x    <- digit
                            ; y    <- many hexDigit
                            ; base <- option 'd' (charIC 'd' <|> charIC 'h' <|> charIC 'q' <|> charIC 'o' <|> charIC 'b')
                            ; let base' = C.toLower base
                            ; let theConst = x : y
                            ; if validConst base' theConst then
                                convertConst base' (T.pack (sign : theConst))
                              else
                                fail "Invalid constant"
                            }
  where
    signChars             = char '+' <|> char '-'
    validConst 'd' x      = and . (map C.isDigit) $ x
    validConst 'o' x      = and . (map C.isOctDigit) $ x
    validConst 'q' x      = validConst 'o' x
    validConst 'h' x      = and . (map C.isHexDigit) $ x
    validConst 'b' []     = True
    validConst 'b' (d:ds) = (d == '0' || d == '1') && validConst 'b' ds
    validConst _   _      = undefined

    convertConst 'd'      = convertConst' decimal
    convertConst 'h'      = convertConst' hexadecimal
    convertConst 'o'      = convertConst' octal
    convertConst 'q'      = convertConst' octal
    convertConst 'b'      = convertConst' binary
    convertConst _        = undefined

    binary txt
      | T.null h  = Left "input does not start with a digit"
      | otherwise = Right (T.foldl' go 0 h, t)
      where (h,t)  = T.span (\c -> c == '0' || c == '1') txt
            go n d = (n * 2 + fromIntegral (digitToInt d))

    octal txt
      | T.null h  = Left "input does not start with a digit"
      | otherwise = Right (T.foldl' go 0 h, t)
      where (h,t)  = T.span C.isDigit txt
            go n d = (n * 8 + fromIntegral (digitToInt d))

    digitToInt d = C.ord d - C.ord '0'
    
    convertConst' cvtFunc x = case signed cvtFunc x of
                                Left errMsg        -> fail errMsg
                                Right (val, "")    -> return $ Const val
                                Right (_val, xtra) -> fail $ T.unpack ( T.concat [ "Extra characters following constant, '"
                                                                                 , xtra
                                                                                 , "'"
                                                                                 ]
                                                                      )

-- | Parse a EDAS label
readLabel :: EDASParser EDASLabel
readLabel =
  let commonSpecial = char '$'
               <|> char '_'
               <|> char '@'
      leadChar =   commonSpecial
               <|> upper
               <|> lower
      followChar = alphaNum
                <|> commonSpecial
                <|> char '?'
      validLabel lab = (T.toUpper lab) `notElem` [ "A", "B", "C", "D", "E", "H", "L" , "I", "R"
                                                 , "IX", "IY", "SP", "AF"
                                                 , "BC", "DE", "HL"
                                                 , "C", "NC", "Z", "NZ", "M", "P", "PE", "PO"
                                                 , "ON", "OFF"
                                                 ]
  in  do { c <- leadChar
         ; lab <- many followChar
         ; let asmLabel = T.cons c (T.pack lab)
         ; if validLabel asmLabel then
             if T.length asmLabel <= 15 then
               return asmLabel
             else
               fail "Labels should be 15 characters or less."
           else
             fail ("Label is a reserved name: " ++ (T.unpack asmLabel))
         }

-- | Parse a case-insensitive string
stringIC :: String
         -> EDASParser String
stringIC icStr = walk icStr
  where
    walk [] = return icStr
    walk (c:cs) = charIC c >> walk cs

-- | Parse a case-insensitive character
charIC :: Char
       -> EDASParser Char
charIC c =
  let showC x           = [ '\'', x, '\'' ]
      testC x           = if C.toLower x == c then Just x else Nothing
      nextPos pos x _xs = updatePosChar pos x
  in  tokenPrim showC nextPos testC

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
