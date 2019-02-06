{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TRS80.Disasm.Guidance
  ( Guidance(..)
  , Directive(..)
  , Z80guidanceAddr(..)
  , Z80guidanceDisp(..)
  , Z80guidanceAddrRange(..)
  , tryit
  , tryit2
  , getKnownSymbols
  , invertKnownSymbols
  , getMatchingSection
  , ToJSON(..)
  , FromJSON(..)
  ) where

import           Control.Applicative  ((<|>))
import           Data.Aeson.Types     (FromJSONKey (..), (.=))
import           Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Char            as C
import           Data.Either          (either, lefts, rights)
import qualified Data.Hashable        as Hashable
import qualified Data.HashMap.Strict  as H
import           Data.Maybe           (fromMaybe, isJust)
import qualified Data.Scientific      as S
import qualified Data.Text            as T
import           Data.Vector          ((!))
import qualified Data.Vector          as V
import           Data.Yaml            (FromJSON (..), Parser, ToJSON (..), object)
import qualified Data.Yaml            as Y
import           Generics.SOP         (ConstructorName, DatatypeName, FieldName)
import           Generics.SOP.JSON    (JsonFieldName, JsonOptions (..), JsonTagName, defaultJsonOptions, gparseJSON, gtoJSON)
import           Generics.SOP.TH      (deriveGeneric)
-- import           Debug.Trace

import           Machine.Utils        (as0xHex, as0xHexS, asHex)
import           Z80                  (Z80addr, Z80disp)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Wrapper type for 'Z80addr' so that we can write our own 'Value' parser
newtype Z80guidanceAddr = Z80guidanceAddr Z80addr
  deriving (Eq, Ord)

instance Show Z80guidanceAddr where
  show (Z80guidanceAddr addr) = as0xHexS addr

-- | Wrapper type for 'Z80disp' so that we can write our own 'Value' parser
newtype Z80guidanceDisp = Z80guidanceDisp Z80disp
  deriving (Eq, Ord)

instance Show Z80guidanceDisp where
  show (Z80guidanceDisp addr) = as0xHexS addr

-- | Wrapper type for address ranges pairs. This accepts "[start address, end address]" or "start", "end"/"nbytes", "nBytes"
-- attribute/value pairs.
data Z80guidanceAddrRange = Z80guidanceAddrRange Z80addr Z80addr
  deriving (Eq, Ord)

instance Show Z80guidanceAddrRange where
  show (Z80guidanceAddrRange sAddr eAddr) = "[" ++ as0xHexS sAddr ++ ", " ++ as0xHexS eAddr ++ "]"

-- | Wrapper type for MD5 signatures
newtype Z80guidanceMD5Sig = Z80guidanceMD5Sig B.ByteString
  deriving (Eq, Show)

-- | Disassembler guidance: When to disassemble, when to dump bytes, ... basically guidance to the drive
-- the disassembly process (could be made more generic as part of a 'Machine' module.)
data Guidance =
  Guidance
    { origin   :: Z80guidanceAddr
      -- Disassembly origin address (where to start disassembling)
    , endAddr  :: Z80guidanceAddr
      -- End disassembly address
    , sections :: H.HashMap T.Text (V.Vector Directive)
      -- Map of section names to a list of directives to apply
    }
  deriving (Eq, Show)

data Directive =
    MD5Sum Z80guidanceMD5Sig
    -- ^ MD5 signature: Conditionally apply the directives iff the section's signature matches
  | SymEquate T.Text Z80guidanceAddr
    -- Symbolic name and value associated iwth with the symbolic name
  | Comment T.Text
    -- Comment text
  | DoDisasm Z80guidanceAddrRange
    -- Start disassembly address and number of bytes to disassemble
  | GrabBytes Z80guidanceAddrRange
    -- Start of range, number of bytes to grab
  | GrabAsciiZ Z80guidanceAddr
    -- Start address to start grabbing 0-terminated ASCII string
  | GrabAscii Z80guidanceAddrRange
    -- Start of range, number of bytes to grab
  | HighBitTable Z80guidanceAddrRange
    -- Start of table, table length
  | JumpTable Z80guidanceAddrRange
    -- Jump table start, table length
  | KnownSymbols (H.HashMap T.Text Z80guidanceAddr)
    -- Mapping between addresses and symbols, more user friendly output
  deriving (Eq, Show)

deriveGeneric ''Directive
deriveGeneric ''Guidance

instance FromJSON Guidance where
  parseJSON = gparseJSON guidanceFields

instance ToJSON Guidance where
  toJSON = gtoJSON guidanceFields

instance FromJSON Directive where
  parseJSON = gparseJSON guidanceFields

instance ToJSON Directive where
  toJSON = gtoJSON guidanceFields

instance FromJSON Z80guidanceAddr where
  parseJSON = repackResult . parseZ80Addr

instance ToJSON Z80guidanceAddr where
  toJSON (Z80guidanceAddr addr) = (Y.String . as0xHex) addr

instance FromJSON Z80guidanceDisp where
  parseJSON = repackResult . parseZ80Disp

instance ToJSON Z80guidanceDisp where
  toJSON (Z80guidanceDisp addr) = (Y.String . as0xHex) addr

-- | Use the default fromJSONKey implementation.
instance FromJSONKey Z80guidanceAddr
-- | And make Z80guidanceAddr hashable (required by 'gparseJSON')
instance Hashable.Hashable Z80guidanceAddr where
  hashWithSalt salt (Z80guidanceAddr addr) = Hashable.hashWithSalt salt addr

-- | Use the default fromJSONKey implementation.
instance FromJSONKey Z80guidanceDisp
-- | Make Z80guidanceDisp hashable (required by 'gparseJSON')
instance Hashable.Hashable Z80guidanceDisp where
  hashWithSalt salt (Z80guidanceDisp addr) = Hashable.hashWithSalt salt addr

instance FromJSON Z80guidanceAddrRange where
  parseJSON (Y.Array vals)
    | V.length vals /= 2
    = fail "Address range must have 2 elements, [start, end]"
    | otherwise
    = yamlZ80addr (\sAddr -> yamlZ80addr (pure . Z80guidanceAddrRange sAddr) (vals ! 1)) (vals ! 0)
  parseJSON (Y.Object attrs) = repackResult (parseStartAddr attrs)
    where
      parseStartAddr attrs' =
        maybe (fail "Missing 'addr' attribute")
              (mkStartEnd attrs')
              (H.lookup "addr" attrs')
      mkStartEnd attrs' {-yamlSAddr-} = yamlZ80addr (parseEndRange attrs')
      parseEndRange attrs' sAddr = maybe (mkStartNBytes attrs' sAddr)
                                         (yamlZ80addr (Right . Z80guidanceAddrRange sAddr))
                                         (H.lookup "end" attrs')
      mkStartNBytes attrs' sAddr =
        maybe (fail "Address range needs 'end' or 'nbytes'/'nBytes'")
              (yamlZ80disp (\nbytes -> Right $ Z80guidanceAddrRange sAddr (sAddr + fromIntegral nbytes)))
              (H.lookup "nbytes" attrs' <|> H.lookup "nBytes" attrs')
  parseJSON _ = fail "Address range requires 'start' and 'end' or 'nbytes'/'nBytes'"

instance ToJSON Z80guidanceAddrRange where
  toJSON (Z80guidanceAddrRange sAddr eAddr) = object ["addr" .= as0xHex sAddr, "nbytes" .= as0xHex (eAddr - sAddr)]

instance FromJSON Z80guidanceMD5Sig where
  parseJSON (Y.String s) =
    let strChunks = T.chunksOf 2 s
        bytes    = map convertHex strChunks
        errs     = lefts bytes
    in  if all (\x -> T.compareLength x 2 == EQ) strChunks && length bytes == 16 && null errs
        then pure $ (Z80guidanceMD5Sig . B.pack . rights) bytes
        else (fail . T.unpack . T.unlines) errs
  parseJSON _            = fail "md5 signature expects a 16 byte hex string"

instance ToJSON Z80guidanceMD5Sig where
  toJSON (Z80guidanceMD5Sig sig) = Y.String $ T.concat $ map asHex $ B.unpack sig

yamlZ80addr :: Monad m
            => (Z80addr -> m a)
            -> Y.Value
            -> m a
yamlZ80addr f y = either (fail . T.unpack)
                         (\(Z80guidanceAddr addr) -> f addr)
                         (parseZ80Addr y)

yamlZ80disp :: Monad m
            => (Z80disp -> m a)
            -> Y.Value
            -> m a
yamlZ80disp f y = either (fail . T.unpack)
                         (\(Z80guidanceDisp addr) -> f addr)
                         (parseZ80Disp y)

repackResult :: Either T.Text a
             -> Parser a
repackResult = either (fail . T.unpack) pure

guidanceFields :: JsonOptions
guidanceFields =
  defaultJsonOptions
  { jsonFieldName  = renameFields
  , jsonTagName    = renameDirective
  }

renameFields :: DatatypeName
               -> FieldName
               -> JsonFieldName
renameFields "Guidance"  "endAddr"  = "end"
renameFields "Guidance"  "sections" = "section"
renameFields "Guidance"  "origin"   = "origin"
renameFields "Directive" fname      = renameDirective fname
renameFields dtName      fname      = dtName ++ ":" ++ fname

renameDirective :: ConstructorName
                -> JsonTagName
renameDirective tag = fromMaybe ("unknown Directive tag: " ++ tag)  $ H.lookup tag directiveRenameTable

-- | Translation table mapping 'Directive' constructor names to tags in the YAML/JSON source
directiveRenameTable :: H.HashMap String String
directiveRenameTable =
  H.fromList [ ("MD5Sum",       "md5")
             , ("SymEquate",    "equate")
             , ("Comment",      "comment")
             , ("DoDisasm",     "disasm")
             , ("GrabBytes",    "bytes")
             , ("GrabAsciiZ",   "asciiz")
             , ("GrabAscii",    "ascii")
             , ("HighBitTable", "highbits")
             , ("JumpTable",    "jumptable")
             , ("KnownSymbols", "symbols")
             ]

parseZ80Addr :: Y.Value
             -> Either T.Text Z80guidanceAddr
parseZ80Addr (Y.String strval) = convertWord16 strval
parseZ80Addr (Y.Number numval) = maybe (outOfRange minZ80addr maxZ80addr numval)
                                            (Right . Z80guidanceAddr)
                                            (S.toBoundedInteger numval)
parseZ80Addr invalid           = fail $ "Invalid Z80 address: " ++ show invalid

parseZ80Disp :: Y.Value
             -> Either T.Text Z80guidanceDisp
parseZ80Disp (Y.String strval)  = convertWord16 strval
parseZ80Disp (Y.Number numval)  = maybe (outOfRange minZ80disp maxZ80disp numval)
                                          (Right . Z80guidanceDisp)
                                          (S.toBoundedInteger numval)
parseZ80Disp invalid            = fail $ "Invalid Z80 displacement: " ++ show invalid

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

convertWord16 :: (Integral a, Bounded a)
              => T.Text
              -> Either T.Text a
convertWord16 t
  | T.isPrefixOf "0x" t
  = convertHex (T.drop 2 t)
  | T.isPrefixOf "0o" t
  = convertOctal (T.drop 2 t)
  | T.isPrefixOf "0" t
  = convertOctal (T.tail t)
  | otherwise
  = convertDecimal t
  | otherwise
  = Left (T.concat ["Invalid 16-bit constant: '", t, singleQuote])

minZ80addr :: Int
minZ80addr = fromIntegral (minBound :: Z80addr)

maxZ80addr :: Int
maxZ80addr = fromIntegral (maxBound :: Z80addr)

minZ80disp :: Int
minZ80disp = fromIntegral (minBound :: Z80disp)

maxZ80disp :: Int
maxZ80disp = fromIntegral (maxBound :: Z80disp)

convertHex :: (Integral a, Bounded a) => T.Text -> Either T.Text a
convertHex t =
  let val = fst $ T.mapAccumR (\v c -> (v * 16 + hexDigit c, c)) 0 $ T.reverse t
      hexDigit c = let i = fromEnum c
                   in  (i .&. 0xf) + ((i .&. 0x40) `shiftR` 6) * 9
      vMin = minBound
      vMax = maxBound
  in if T.all (\c -> let c' = fromEnum c
                             in (c' >= fromEnum '0' && (c' <= fromEnum '9')) ||
                                (c' >= fromEnum 'a' && (c' <= fromEnum 'f')) ||
                                (c' >= fromEnum 'A' && (c' <= fromEnum 'F'))) t
      then if val >= vMin && val <= vMax           then Right $ fromIntegral val
           else outOfRange vMin vMax t
     else Left $ T.concat ["Invalid hexadecimal constant: '", t, singleQuote]

convertOctal :: (Integral a, Bounded a) => T.Text -> Either T.Text a
convertOctal octstr =
  let val = fst $ T.mapAccumR (\v c -> (v * 8 + (fromEnum c .&. 0xf), c)) 0 (T.reverse octstr)
      validOctal = T.all (\c -> let c' = fromEnum c
                                in  (c' >= fromEnum '0' && c' <= fromEnum '7'))
      vMin = minBound
      vMax = maxBound
  in  if validOctal octstr
      then if val >= vMin && val <= vMax
           then (Right . fromIntegral) val
           else outOfRange minBound maxBound octstr
      else Left $ T.concat ["Invalid octal constant: '", octstr, singleQuote]

convertDecimal :: (Integral a, Bounded a) => T.Text -> Either T.Text a
convertDecimal str =
  let val = fst $ T.mapAccumR (\v c -> (v * 10 + (fromEnum c .&. 0xf), c)) 0 (T.reverse str)
      vMin = minBound
      vMax = maxBound
  in  if T.all C.isDigit str
      then if val >= vMin && val <= vMax
           then Right (fromIntegral val)
           else outOfRange minBound maxBound str
      else Left $ T.concat ["Invalid constant: '", str, singleQuote]

{-
validSymName :: T.Text -> Bool
validSymName sym = let validChar x = (C.isLetter x || x == '$' || x == '_' || x == '@')
                   in  (validChar . T.head) sym
                       && T.compareLength sym 15 /= GT
                       && T.all (\x -> validChar x || C.isDigit x || x == '?') (T.tail sym)
-}

outOfRange :: (Show a) => Int -> Int -> a -> Either T.Text b
outOfRange minRange maxRange thing = Left $ T.concat ["Value range exceeded ("
                                                     , T.pack (show minRange)
                                                     , " <= x <= "
                                                     , T.pack (show maxRange)
                                                     , "): "
                                                     , T.pack (show thing)
                                                     ]

singleQuote :: T.Text
singleQuote = T.singleton '\''

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

getKnownSymbols :: V.Vector Directive
                -> H.HashMap T.Text Z80guidanceAddr
getKnownSymbols dirs = let knownSymbols (KnownSymbols _) = True
                           knownSymbols _                = False
                       in  maybe H.empty
                                 (\(KnownSymbols syms) -> syms)
                                 (V.find knownSymbols dirs)

invertKnownSymbols :: V.Vector Directive
                   -> H.HashMap Z80addr T.Text
invertKnownSymbols guidance = H.fromList $ map flipSymAddr $ H.toList $ getKnownSymbols guidance
  where
    flipSymAddr (sym, Z80guidanceAddr addr)  = (addr, sym)

getMatchingSection :: Guidance
                   -> B.ByteString
                   -> Maybe (V.Vector Directive)
getMatchingSection g md5sum =
  let matchesMD5 dirs = isJust $ V.find (\case
                                          (MD5Sum (Z80guidanceMD5Sig md5)) -> md5sum == md5
                                          _                                -> False) dirs
      filteredSects   = H.filter matchesMD5 (sections g)
      sectKeys        = H.keys filteredSects
  in  if not (null filteredSects) && length sectKeys == 1
      then Just (filteredSects H.! head sectKeys)
      else Nothing

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

instance Integral Z80guidanceAddr where
  quotRem (Z80guidanceAddr addr) (Z80guidanceAddr divisor)   = (Z80guidanceAddr q, Z80guidanceAddr r)
    where (q, r) = quotRem addr divisor
  toInteger (Z80guidanceAddr addr) = toInteger addr

instance Enum Z80guidanceAddr where
  toEnum addr = Z80guidanceAddr $ fromIntegral addr
  fromEnum (Z80guidanceAddr addr) = fromIntegral addr

instance Num Z80guidanceAddr where
  (Z80guidanceAddr l) + (Z80guidanceAddr r) = Z80guidanceAddr (l + r)
  (Z80guidanceAddr l) * (Z80guidanceAddr r) = Z80guidanceAddr (l * r)
  abs (Z80guidanceAddr x) = Z80guidanceAddr (abs x)
  signum (Z80guidanceAddr x) = Z80guidanceAddr (signum x)
  fromInteger x = Z80guidanceAddr (fromInteger x)
  negate (Z80guidanceAddr x) = Z80guidanceAddr (negate x)

instance Real Z80guidanceAddr where
  toRational (Z80guidanceAddr addr) = toRational addr

instance Bounded Z80guidanceAddr where
  minBound = Z80guidanceAddr (minBound :: Z80addr)
  maxBound = Z80guidanceAddr (maxBound :: Z80addr)

-- And its instances:
instance Integral Z80guidanceDisp where
  quotRem (Z80guidanceDisp addr) (Z80guidanceDisp divisor)   = (Z80guidanceDisp q, Z80guidanceDisp r)
    where (q, r) = quotRem addr divisor
  toInteger (Z80guidanceDisp addr) = toInteger addr

instance Enum Z80guidanceDisp where
  toEnum addr = Z80guidanceDisp $ fromIntegral addr
  fromEnum (Z80guidanceDisp addr) = fromIntegral addr

instance Num Z80guidanceDisp where
  (Z80guidanceDisp l) + (Z80guidanceDisp r) = Z80guidanceDisp (l + r)
  (Z80guidanceDisp l) * (Z80guidanceDisp r) = Z80guidanceDisp (l * r)
  abs (Z80guidanceDisp x) = Z80guidanceDisp (abs x)
  signum (Z80guidanceDisp x) = Z80guidanceDisp (signum x)
  fromInteger x = Z80guidanceDisp (fromInteger x)
  negate (Z80guidanceDisp x) = Z80guidanceDisp (negate x)

instance Real Z80guidanceDisp where
  toRational (Z80guidanceDisp addr) = toRational addr

instance Bounded Z80guidanceDisp where
  minBound = Z80guidanceDisp (minBound :: Z80disp)
  maxBound = Z80guidanceDisp (maxBound :: Z80disp)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Diagnostic functions:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

tryit :: IO (Either Y.ParseException Guidance)
tryit = Y.decodeFileEither "/tmp/guidance.yaml"

tryit2 :: IO ()
tryit2 = tryit >>= either print (Y.encodeFile "/tmp/out.yaml")
