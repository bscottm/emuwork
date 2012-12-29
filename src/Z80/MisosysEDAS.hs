{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Misosys EDAS assembler parser. There are two basic interfaces: 'edasParseOneLine' and 'edasParseSequence'. 'edasParseOneLine'
-- is intended to be used as a combinator, e.g., with parsing the analytic disassembly output.
module Z80.MisosysEDAS
  ( edasParseOneLine
  , edasParseSequence
  , edasParseFile
  ) where

import Debug.Trace

import Control.Exception hiding (try)
import Text.Parsec
import Text.Parsec.Text
-- import Text.Parsec.Char
import Text.Parsec.Pos
import Data.Text.Read
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C

import Z80.InstructionSet

-- | Parse an EDAS source file
edasParseFile :: FilePath
              -> IO (Either T.Text [EDAS a])
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
              -> Either T.Text (EDAS a)
edasParseOneLine path inp =
  case parse parseEDAS path inp of
    Left errmsg      -> Left $ ((T.pack . show) errmsg)
    (Right result)   -> Right result

-- | Parse multiple lines of EDAS assembly
edasParseSequence :: FilePath
                  -> T.Text
                  -> Either T.Text [(EDAS a)]
edasParseSequence path inp =
  let manyEDASlines = do { result <- many parseEDAS
                         ; return result
                         }
  in  case parse manyEDASlines path inp of
        Left errmsg      -> Left $ ((T.pack . show) errmsg)
        (Right result)   -> Right result

-- | Synonym for an assembler label
type EDASLabel = T.Text

-- | An assembler comment, preserving its column position in the input
data Comment = Comment Int T.Text
  deriving (Show)

-- | Assembler operation: a Z80 instruction or EDAS pseudo operation
data AsmOp a where
  Insn   :: Z80instruction              -- An instruction
         -> AsmOp a
  Pseudo :: EDASPseudo a                -- A pseudo operation, e.g., equate
         -> AsmOp a
  deriving (Show)

-- | EDAS' pseudo operations
data EDASPseudo a where
  NullPseudo :: EDASPseudo a
  Equate :: EDASExpr a
         -> EDASPseudo a
  Origin :: EDASExpr a
         -> EDASPseudo a
  deriving (Show)

-- | Expressions
data EDASExpr a where
  Const :: Int16                -- 16-bit integer constant
        -> EDASExpr a
  Var   :: T.Text               -- Variable/symbolic label
        -> EDASExpr a
  Add   :: EDASExpr a
        -> EDASExpr a
        -> EDASExpr a
  Sub   :: EDASExpr a
        -> EDASExpr a
        -> EDASExpr a
  Mul   :: EDASExpr a
        -> EDASExpr a
        -> EDASExpr a
  Div   :: EDASExpr a
        -> EDASExpr a
        -> EDASExpr a
  Mod   :: EDASExpr a
        -> EDASExpr a
        -> EDASExpr a
  Shift :: EDASExpr a
        -> EDASExpr a
        -> EDASExpr a

instance Show (EDASExpr a) where
  show (Const x)   = "Const(" ++ (show x) ++ ")"
  show (Var v)     = "Var(" ++ (show v) ++ ")"
  show (Add x y)   = "Add(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Sub x y)   = "Sub(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Mul x y)   = "Mul(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Div x y)   = "Div(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Mod x y)   = "Mod(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Shift x y) = "Shift(" ++ (show x) ++ "," ++ (show y) ++ ")"

-- | Data type constructors for EDAS data elements
data EDAS a where
  NoResult  :: EDAS a
  -- Assembler statement
  AsmStmt :: { _symLabel :: Maybe EDASLabel
             , _asmOp    :: Maybe (AsmOp a)
             , _comment  :: Maybe Comment
             } -> EDAS a
  deriving (Show)

-- | Make an empty assembler statement
mkAsmStmt :: EDAS a
mkAsmStmt = AsmStmt { _symLabel = Nothing
                    , _asmOp    = Nothing
                    , _comment  = Nothing
                    }

-- | The EDAS parser for each line of input.
parseEDAS :: Parser (EDAS a)
parseEDAS = asmStatement

-- Skip over whitespace. NB that the newline is important, so 'spaces' is not used here since it will skip over the
-- newline.
whiteSpace :: Parser ()
whiteSpace = skipMany (oneOf [' ', '\t', '\f', '\v'])

-- | Parse a comment, preserving its position in the input.
asmComment :: Parser Comment
asmComment =
  getPosition
  >>= (\curpos ->
        let srccol = sourceColumn curpos
        in  try (char ';')
            >> ( manyTill anyChar (try newline)
                 >>= (\cmnt -> return $ Comment srccol (T.pack cmnt))
               )
      )

-- | Parse an assembler statement, which has the form "label: op operands ; comment"
asmStatement :: Parser (EDAS a)
asmStatement =
  let updateStmtLab stmtLab asmStmt = asmStmt { _symLabel = stmtLab }
      updateAsmOp   asmOp   asmStmt = asmStmt { _asmOp    = asmOp   }
      updateComment cmnt    asmStmt = asmStmt { _comment  = cmnt    }

      parseAsmOp                    = do { asmOp <- ( insnStmt
                                                      <|> pseudoOpStmt 
                                                    ) <?> "valid Z80 instruction or pseudo-operation"
                                         ; optional whiteSpace
                                         ; return asmOp
                                         }
      insnStmt                      = do { _ <- char ':' <?> "':' expected after label"
                                         ; optional whiteSpace
                                         ; asmOpcode     <?> "Z80 instruction"
                                         }
      pseudoOpStmt                  = do { optional whiteSpace
                                         ; asmPseudo     <?> "pseudo-operation"
                                         }
      parseComment                  = do { _ <- newline
                                         ; return $ Nothing 
                                         }
                                      <|> do { cmnt <- asmComment
                                             ; return $ Just cmnt
                                             }
  in  do { stmtLab <- optionMaybe readLabel
         ; asmOp   <- optionMaybe parseAsmOp
         ; cmnt    <- parseComment
         ; return (((updateStmtLab stmtLab) . (updateAsmOp asmOp) . (updateComment cmnt)) mkAsmStmt)
         }

-- | Parse a Z80 assembler opcode
asmOpcode :: Parser (AsmOp a)
asmOpcode = 
  do { stringIC "ld"
     ; return $ (Insn $ Z80undef [])
     }

-- | Parse EDAS pseudo operations, such as "EQU", "DEFS", etc.
asmPseudo :: Parser (AsmOp a)
asmPseudo =
  (\pseudoOp -> return $ Pseudo pseudoOp)
  =<< ( pseudoWithExpr "equ" Equate
        <|> pseudoWithExpr "org" Origin
      )
  where
    pseudoWithExpr str pseudo = do { stringIC str
                                   ; optional whiteSpace
                                   ; expr <- asmExpr
                                   ; return $ pseudo expr
                                   }

-- | Assembler expression parsing
asmExpr :: Parser (EDASExpr a)
asmExpr =
  constExpr
    <|> varExpr
    <|> binOp '+' Add
    <|> binOp '-' Sub
    <|> binOp '*' Mul
    <|> binOp '/' Div
    <|> binOpSI ".mod." Mod
    <|> binOp '<' Shift
  where
    -- constant expression: Optional sign, digits and base
    constExpr = do { sign <- option '+' (char '+' <|> char '-')
                   ; x    <- many hexDigit
                   ; base <- option 'd' (charIC 'd' <|> charIC 'h' <|> charIC 'q' <|> charIC 'o' <|> charIC 'b')
                   ; if validConst base x then
                       convertConst base (T.pack (sign:x))
                     else
                       fail "Invalid constant"
                   }
    varExpr    = (\varName -> return $ Var varName)
                 =<< readLabel

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

    binOp c op = do { left  <- asmExpr
                    ; optional whiteSpace
                    ; _     <- char c
                    ; optional whiteSpace
                    ; right <- asmExpr
                    ; return $ op left right
                    }

    binOpSI str op = do { left  <- asmExpr
                        ; optional whiteSpace
                        ; _     <- stringIC str
                        ; optional whiteSpace
                        ; right <- asmExpr
                        ; return $ op left right
                        }

-- | Parse a case-insensitive string
stringIC :: forall s (m :: * -> *). Stream s m Char =>
            String
         -> ParsecT s () m ()
stringIC [] = return ()
stringIC (c:cs) = charIC c >> stringIC cs

-- | Parse a case-insensitive character
charIC :: forall s (m:: * -> *). Stream s m Char =>
          Char
       -> ParsecT s () m Char
charIC c =
  let showC x           = [ '\'', x, '\'' ]
      testC x           = if C.toLower x == c then Just x else Nothing
      nextPos pos x _xs = updatePosChar pos x
  in  tokenPrim showC nextPos testC

-- | Parse a EDAS label
readLabel :: Parser EDASLabel
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
      validLabel lab = (T.toUpper lab) `notElem` [ "A", "B", "C", "D", "E", "H", "L"
                                                 , "I", "R", "IX", "IY", "SP", "AF"
                                                 , "BC", "DE", "HL", "C", "NC", "Z"
                                                 , "NZ", "M", "P", "PE", "PO", "ON", "OFF"
                                                 ]
  in  try $
        do
          c <- leadChar
          lab <- many followChar
          let asmLabel = T.cons c (T.pack lab)
          if validLabel asmLabel then
            if T.length asmLabel <= 15 then
              return asmLabel
            else
              fail "Labels should be 15 characters or less."
          else
            fail ("Label is a reserved name: " ++ (T.unpack asmLabel))

-- | Generic I/O exception handler. This outputs a message to 'stderr' of the form
--
-- @whence: exception@
genericIOErrors :: T.Text                       -- ^ An indication of where the error ocurred
                -> IOError                      -- ^ The IO exception
                -> IO (Either T.Text [EDAS a])  -- ^ Result is always 'invalidVector'
genericIOErrors whence exc = return $ Left $ T.concat [ whence
                                                      , ": "
                                                      , ((T.pack . show) exc)
                                                      ]
