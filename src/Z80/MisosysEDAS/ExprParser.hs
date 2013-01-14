{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Expression parsing for the Misosys EDAS-compatible assembler
module Z80.MisosysEDAS.ExprParser
 ( asmExpr
 , constExpr
 ) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Text()                 -- ghci 7.6.1 needs these imported instances, e.g., (Stream T.Text Identity Char)
import Data.Text.Read
import qualified Data.Text as T
import qualified Data.Char as C

import Z80.MisosysEDAS.Types
import Z80.MisosysEDAS.ParserUtils

-- | Assembler expression parsing. The expression must minimally have a primary
-- expression and may be optionally followed by binary operator expressions.
--
-- Primary expression: constant, variable/symbolic label name, @$@ for the current program counter, 
-- or unuary operator (@.not.@, @.high.@ or @.low.@) followed by a primary expression
--
-- EDAS did not have a concept of operator precedence or parenthesized expressions, which is mimicked here as well.
asmExpr :: EDASParser EDASExpr
asmExpr =
  do { leftExpr <- primExpr
     ; binOps leftExpr
     }
  where
    -- Primary expressions: constants, variables/symbols/labels, unary operators
    primExpr = getPosition
               >>= (\srcpos -> constExpr False srcpos
                               <|> try ( char '$'
                                         >> notFollowedBy readLabel
                                         >> return CurrentPC
                                       )
                               <|> try ( getPosition
                                         >>= (\labSrcPos -> liftM (Var labSrcPos) readLabel)
                                       )
                               <|> try ( unaryOp "not" OnesCpl
                                         <|> unaryOp "high" HighByte
                                         <|> unaryOp "low" LowByte
                                       )
                               <|> try ( liftM AsmChar (between (char '\'') (char '\'') asciiChar) )
                   )

    -- Thankfully, EDAS makes parsing unary operations easy by putting them between '.'s:
    unaryOp opName opCtor = try ( do { _ <- between (char '.') (char '.') (stringIC opName)
                                     ; optional whiteSpace
                                     ; liftM opCtor primExpr
                                     }
                                )

    binOps leftExpr = binOp leftExpr '+' Add
                      <|> binOp leftExpr '-' Sub
                      <|> binOp leftExpr '*' Mul
                      <|> binOp leftExpr '/' Div
                      <|> binOp leftExpr '<' Shift
                      <|> binOp leftExpr '&' LogAnd
                      <|> binOp leftExpr '!' LogOr
                      <|> binOpWord leftExpr "and" LogAnd
                      <|> binOpWord leftExpr "or" LogOr
                      <|> binOpWord leftExpr "mod" Mod
                      <|> binOpWord leftExpr "xor" LogXor
                      <|> binOpWord leftExpr "eq" LogEQ
                      <|> binOpWord leftExpr "ge" LogGE
                      <|> binOpWord leftExpr "gt" LogGT
                      <|> binOpWord leftExpr "le" LogLE
                      <|> binOpWord leftExpr "lt" LogLT
                      <|> binOpWord leftExpr "shl" ShiftL
                      <|> binOpWord leftExpr "shr" ShiftR
                      <|> return leftExpr                     -- None of the parsers matched, so return the expression

    binOp leftExpr c op = try ( do { optional whiteSpace
                                   ; _ <- char c
                                   ; optional whiteSpace
                                   ; rightExpr <- primExpr
                                   ; let term = leftExpr `op` rightExpr
                                   ; binOps term
                                 }
                              )

    binOpWord leftExpr str op = try ( do { optional whiteSpace
                                         ; _  <- between (char '.') (char '.') (stringIC str)
                                         ; optional whiteSpace
                                         ; rightExpr <- primExpr
                                         ; let term = leftExpr `op` rightExpr
                                         ; binOps term
                                         }
                                   )

-- | Constant expression parser: Optional sign, digits and base, e.g., @0A100H@ is a hex constant. Bases are @/d/@ for decimal,
-- @/h/@ for hex, @/o/@ or @/q/@ for octal and @/b/@ for binary.
constExpr :: Bool                               -- ^ Sign required, 'True' when parsing IX, IY displacements
          -> SourcePos
          -> EDASParser EDASExpr                -- ^ Result is an 'EDASExpr'
constExpr signRequired srcpos =
  do { sign <- if signRequired then
                 signChars
               else
                 option '+' signChars
     ; x    <- digit
     ; (y, base) <- ( try ( do { ds <- many hexDigit
                               ; b  <- charIC 'h'
                               ; return (ds, b)
                               }
                          )
                      <|> try ( do { ds <- many octDigit
                                   ; b  <- (charIC 'o' <|> charIC 'q')
                                   ; return (ds, b)
                                   }
                              )
                      <|> try ( do { ds <- many (satisfy (\d -> d == '0' || d == '1'))
                                   ; b  <- charIC 'b'
                                   ; return (ds, b)
                                   }
                              )
                      <|> try ( do { ds <- many digit
                                   ; b  <- option 'd' (charIC 'd')
                                   ; return (ds, b)
                                   }
                              )
                    )
     ; convertConst (C.toLower base) (T.pack (sign : x : y))
     } <?> "decimal, hex, octal or binary constant"
  where
    signChars             = char '+' <|> char '-'

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
                                Right (val, "")    -> return $ Const srcpos val
                                Right (_val, xtra) -> fail $ T.unpack ( T.concat [ "Extra characters following constant, '"
                                                                                 , xtra
                                                                                 , "'"
                                                                                 ]
                                                                      )
