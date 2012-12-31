-- | Misosys EDAS assembler parser. There are two basic interfaces: 'edasParseOneLine' and 'edasParseSequence'. 'edasParseOneLine'
-- is intended to be used as a combinator, e.g., with parsing the analytic disassembly output.
module Z80.MisosysEDAS
  ( edasParseOneLine
  , edasParseSequence
  , edasParseFile
  ) where

import Debug.Trace

import Control.Exception hiding (try)
import Control.Lens
import Text.Parsec
import Text.Parsec.Pos
import Data.Text.Read
import Data.Int
import Data.Word
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Machine.EmulatedSystem
import Z80.InstructionSet

-- | Synonym for an assembler label
type EDASLabel = T.Text

-- | An assembler comment, preserving its column position in the input
data Comment = Comment Int T.Text
  deriving (Show)

-- | Assembler operation: a Z80 instruction or EDAS pseudo operation
data AsmOp where
  Insn   :: (AsmEvalCtx -> Z80instruction)      -- An instruction constructor, may involve an expression
                                                -- and an evaluation context
         -> AsmOp
  Pseudo :: EDASPseudo                  -- A pseudo operation, e.g., equate, db, ds, etc.
         -> AsmOp
  deriving (Show)

instance Show (AsmEvalCtx -> Z80instruction) where
  show _ctxInsn = "<asm eval>"

-- | EDAS' pseudo operations
data EDASPseudo where
  NullPseudo :: EDASPseudo
  Equate :: EDASExpr
         -> EDASPseudo
  Origin :: EDASExpr
         -> EDASPseudo
  deriving (Show)

-- | Expressions
data EDASExpr where
  NullExpr :: EDASExpr          -- Null/no expression
  Const :: Int16                -- 16-bit integer constant
        -> EDASExpr
  Var   :: T.Text               -- Variable/symbolic label
        -> EDASExpr
  Add   :: EDASExpr
        -> EDASExpr
        -> EDASExpr
  Sub   :: EDASExpr
        -> EDASExpr
        -> EDASExpr
  Mul   :: EDASExpr
        -> EDASExpr
        -> EDASExpr
  Div   :: EDASExpr
        -> EDASExpr
        -> EDASExpr
  Mod   :: EDASExpr
        -> EDASExpr
        -> EDASExpr
  Shift :: EDASExpr
        -> EDASExpr
        -> EDASExpr
  deriving (Show)

-- | Assembler evaluation context
data AsmEvalCtx where
  AsmEvalCtx :: { _symbolTab :: Map T.Text AsmSymbolVal
                } -> AsmEvalCtx

-- | Assembler symbol values
data AsmSymbolVal = AsmSymbolVal EDASExpr (Maybe Int16)

-- | Data type constructors for EDAS data elements
data AsmStmt where
  -- Assembler statement
  AsmStmt :: { _symLabel :: Maybe EDASLabel
             , _asmOp    :: Maybe AsmOp
             , _comment  :: Maybe Comment
             , _stmtAddr :: Word16                      -- Statement address, i.e., current program counter
             } -> AsmStmt
  deriving (Show)

-- Emit TH lens hair:
makeLenses ''AsmStmt

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

-- | Parse a single line of EDAS assembly
edasParseOneLine :: FilePath
                 -> T.Text
                 -> Either T.Text AsmStmt
edasParseOneLine path inp =
  case runParser asmStatement mkEmptyAsmStmt path inp of
    Left errmsg      -> Left $ ((T.pack . show) errmsg)
    Right result     -> Right result

-- | Parse multiple lines of EDAS assembly
edasParseSequence :: FilePath
                  -> T.Text
                  -> Either T.Text [AsmStmt]
edasParseSequence path inp =
  let manyEDASlines = do { result <- many asmStatement
                         ; return result
                         }
  in  case runParser manyEDASlines mkEmptyAsmStmt path inp of
        Left errmsg  -> Left $ ((T.pack . show) errmsg)
        Right result -> Right result

-- | Make an empty assembler statement
mkEmptyAsmStmt :: AsmStmt
mkEmptyAsmStmt = AsmStmt { _symLabel = Nothing
                         , _asmOp    = Nothing
                         , _comment  = Nothing
                         , _stmtAddr = 0
                         }

-- Skip over whitespace. NB that the newline is important, so 'spaces' is not used here since it will skip over the
-- newline.
whiteSpace :: ParsecT T.Text AsmStmt Identity ()
whiteSpace = skipMany (oneOf [' ', '\t', '\f', '\v'])

-- | Parse a comment, preserving its position in the input.
asmComment :: ParsecT T.Text AsmStmt Identity Comment
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
asmStatement :: ParsecT T.Text AsmStmt Identity AsmStmt
asmStatement =
  let updateStmtLab stmtLab asmStmt = asmStmt { _symLabel = stmtLab }
      updateAsmOp   asmOp   asmStmt = asmStmt { _asmOp    = asmOp   }
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
                                                  do { _ <- char ':'
                                                     ; optional whiteSpace
                                                     ; optionMaybe asmOpcode        <?> "Z80 instruction"
                                                     }
                                                      -- Otherwise, we should expect an instruction or pseudo-op
                                                  <|> do { whiteSpace
                                                         ; maybeAsmOrPseudo         <?> "Z80 instruction or pseudo-operation"
                                                         }
                                                Nothing     ->
                                                  -- No label: expect an instruction or pseudo-op
                                                  maybeAsmOrPseudo
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
         ; trace ("asmStmt: " ++ (show ustate)) $ (return ustate)
         }

-- | Parse a Z80 assembler opcode
asmOpcode :: ParsecT T.Text AsmStmt Identity AsmOp
asmOpcode = 
  do { _ <- stringIC "ld"
     ; _ <- whiteSpace
     -- Need to handle operands involving the accumulator separately from other 8-bit registers
     ; reg8immLoads
     }
  <|> do { _ <- stringIC "call"
         ; _ <- whiteSpace
         ; calldest <- asmExpr
         ; return $ Insn (\ctx -> (CALL . AbsAddr) $ evalAsmExprToWord16 ctx calldest)
         }
  <|> do { _ <- stringIC "ret"
         ; noArgs RET
         }
  <|> do { _ <- stringIC "nop"
         ; noArgs NOP
         }
  where
    returnInsn = return . Insn
    noArgs z80insn = returnInsn (\_ctx -> z80insn)

    -- Parse accumulator loads.
    reg8immLoads = do { dstReg <- choice (map (stringIC . T.unpack) (Map.keys reg8Names))
                      ; optional whiteSpace
                      ; _ <- char ','
                      ; optional whiteSpace
                      ; expr <- asmExpr
                      ; returnInsn (\ctx -> LD (Reg8Imm (reg8Names ! (T.toLower . T.pack) dstReg) (evalAsmExprToWord8 ctx expr)))
                      }

-- | Parse EDAS pseudo operations, such as "EQU", "DEFS", etc.
asmPseudo :: ParsecT T.Text AsmStmt Identity AsmOp
asmPseudo =
  (\pseudoOp -> return $ Pseudo pseudoOp)
  =<< ( pseudoWithExpr "equ" Equate
        <|> pseudoWithExpr "org" Origin
      )
  where
    pseudoWithExpr str pseudo = do { _ <- stringIC str
                                   ; whiteSpace
                                   ; expr <- asmExpr
                                   ; return $ pseudo expr
                                   }

-- | Assembler expression parsing
asmExpr :: ParsecT T.Text AsmStmt Identity EDASExpr
asmExpr =
  do { prim <- primExpr
     ; optional whiteSpace
     ; result <- option prim ( binOp prim '+' Add
                                 <|> binOp prim '-' Sub
                             )
     ; return result
     }
  where
    -- Primary expression
    primExpr = constExpr
                 <|> varExpr

    -- constant expression: Optional sign, digits and base
    constExpr = do { sign <- option '+' (char '+' <|> char '-')
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
    varExpr    = (\varName -> return $ Var varName) =<< readLabel

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

    binOp leftExpr c op = do { _     <- char c
                             ; optional whiteSpace
                             ; rightExpr <- asmExpr
                             ; return $ op leftExpr rightExpr
                             }

    binOpSI leftExpr str op = do { _  <- stringIC str
                                 ; optional whiteSpace
                                 ; rightExpr <- asmExpr
                                 ; return $ op leftExpr rightExpr
                                 }

-- | Parse a EDAS label
readLabel :: ParsecT T.Text AsmStmt Identity EDASLabel
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
         -> ParsecT T.Text AsmStmt Identity String
stringIC icStr = walkIC icStr
  where
    walkIC [] = return icStr
    walkIC (c:cs) = charIC c >> walkIC cs

-- | Parse a case-insensitive character
charIC :: Char
       -> ParsecT T.Text AsmStmt Identity Char
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

-- | Evaluate an assembler expression to produce a Word16 result, within the current assembler evaluation context
evalAsmExprToWord16 :: AsmEvalCtx
                    -> EDASExpr
                    -> Word16
evalAsmExprToWord16 = undefined

evalAsmExprToWord8 :: AsmEvalCtx
                   -> EDASExpr
                   -> Word8
evalAsmExprToWord8 = undefined
