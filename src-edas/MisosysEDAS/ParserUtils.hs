{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Parser utility functions shared across the various parsing sources
module Z80.MisosysEDAS.ParserUtils
  ( stringIC
  , charIC
  , asciiChar
  , whiteSpace
  , readLabel
  ) where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Text()               -- ghci needs these instances...
import qualified Data.Text as T
import qualified Data.Char as C

import Z80.MisosysEDAS.Types

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

-- | Parse any ASCII character
asciiChar :: EDASParser Char
asciiChar =
  let showC x           = [ '\'', x, '\'' ]
      testC x           = let x' = C.ord x
                          in  if x' >= 0x20 && x' <= 0x7e then 
                                Just x
                              else
                                Nothing
      nextPos pos x _xs = updatePosChar pos x
  in  tokenPrim showC nextPos testC

-- | Skip over whitespace. NB that the newline is important, so 'spaces' is not used here since it will skip over the
-- newline.
whiteSpace :: EDASParser ()
whiteSpace = skipMany (oneOf [' ', '\t', '\f', '\v'])

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
