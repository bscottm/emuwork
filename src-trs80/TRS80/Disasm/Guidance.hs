{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
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
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson.Types               ( (.=) )
import qualified Data.Aeson.KeyMap as AKM       ( lookup )
import           Data.Bits
import qualified Data.ByteString.Lazy          as B
import qualified Data.Char                     as C
import           Data.Either                    ( lefts
                                                , rights
                                                )
import qualified Data.HashMap.Strict           as H
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , fromJust
                                                )
import qualified Data.Scientific               as S
import qualified Data.Text                     as T
import           Data.Vector                    ( (!) )
import qualified Data.Vector                   as V
import           Data.Yaml                      ( FromJSON(..)
                                                , Parser
                                                , ToJSON(..)
                                                , object
                                                )
import qualified Data.Yaml                     as Y
import           Generics.SOP                   ( ConstructorName
                                                , DatatypeName
                                                , FieldName
                                                )
import           Generics.SOP.JSON              ( JsonFieldName
                                                , JsonOptions(..)
                                                , JsonTagName
                                                , defaultJsonOptions
                                                , gparseJSON
                                                , gtoJSON
                                                )
import           Generics.SOP.TH                ( deriveGeneric )
-- import           Debug.Trace

import           Machine.Utils                  ( ShowHex(..)
                                                , as0xHex
                                                , as0xHexS
                                                , asHex
                                                )
import           Z80                            ( Z80addr
                                                , Z80disp
                                                )

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- <sigh> orphan instance...
instance e ~ T.Text => MonadFail (Either e) where
  fail = Left . T.pack

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Wrapper type for 'Z80addr' so that we can write our own 'Value' parser
data Z80guidanceAddr =
    FromCurPC
  |  GuidanceAddr Z80addr
  deriving (Eq)

instance Show Z80guidanceAddr where
  show FromCurPC           = "$"
  show (GuidanceAddr addr) = as0xHexS addr

instance ShowHex Z80guidanceAddr where
  asHex FromCurPC           = "$"
  asHex (GuidanceAddr addr) = as0xHex addr

-- | Wrapper type for 'Z80disp' so that we can write our own 'Value' parser
newtype Z80guidanceDisp =
  Z80guidanceDisp
  {
    unZ80guidanceDisp :: Z80disp
  } deriving (Eq, Ord)

instance Show Z80guidanceDisp where
  show = as0xHexS . unZ80guidanceDisp

-- | Wrapper type for address ranges pairs. This accepts "[start address, end address]" or "start", "end"/"nbytes", "nBytes"
-- attribute/value pairs.
data Z80guidanceAddrRange =
    AbsRange Z80guidanceAddr Z80guidanceAddr
  -- ^ Absolute range, where the end address is absolute
  | RelRange Z80guidanceAddr Z80guidanceDisp
  -- ^ Relative range, wher ethe end address is a displacement (i.e., relative)
  deriving (Eq)

instance Show Z80guidanceAddrRange where
  show (AbsRange sAddr eAddr) = "AbsRange(" ++ as0xHexS sAddr ++ ", " ++ as0xHexS eAddr ++ ")"
  show (RelRange sAddr disp ) = "RelRange(" ++ as0xHexS sAddr ++ ", " ++ (as0xHexS . unZ80guidanceDisp) disp ++ ")"

-- | Wrapper type for MD5 signatures
newtype Z80guidanceMD5Sig =
  Z80guidanceMD5Sig {
    unZ80guidanceMD5Sig :: B.ByteString
  } deriving (Eq, Show)

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
    -- Mapping between symbols and addresses for a more user friendly output
  deriving (Eq, Show)

deriveGeneric ''Directive
deriveGeneric ''Guidance

instance FromJSON Guidance where
  parseJSON json = gparseJSON guidanceFields json >>= check
    where
      check guidance
        | origin guidance == FromCurPC
        = fail "Disassembly origin cannot be '$'."
        | endAddr guidance == FromCurPC
        = fail "Disassembly end cannot be '$'."
        | otherwise
        = pure guidance

instance ToJSON Guidance where
  toJSON = gtoJSON guidanceFields

instance FromJSON Directive where
  parseJSON = gparseJSON guidanceFields

instance ToJSON Directive where
  toJSON = gtoJSON guidanceFields

instance FromJSON Z80guidanceAddr where
  parseJSON = repackResult . parseZ80guidanceAddr

instance ToJSON Z80guidanceAddr where
  toJSON FromCurPC           = Y.String "$"
  toJSON (GuidanceAddr addr) = (Y.String . as0xHex) addr

instance FromJSON Z80guidanceDisp where
  parseJSON = repackResult . parseZ80Disp

instance ToJSON Z80guidanceDisp where
  toJSON = Y.String . as0xHex . unZ80guidanceDisp

instance FromJSON Z80guidanceAddrRange where
  parseJSON (Y.Array vals)
    | V.length vals /= 2 = fail "Address range must have 2 elements, [start, end]"
    | otherwise          = repackResult $ AbsRange <$> parseZ80guidanceAddr (vals ! 1) <*> parseZ80guidanceAddr (vals ! 0)
  parseJSON (Y.Object attrs) = repackResult $ parseAddrRange attrs
  parseJSON _                = fail "Address range should have 'start' and 'end' or 'nbytes'/'nBytes' attributes"

instance ToJSON Z80guidanceAddrRange where
  toJSON (AbsRange sAddr eAddr) = object ["addr" .= as0xHex sAddr, "end" .= as0xHex eAddr]
  toJSON (RelRange sAddr disp ) = object ["addr" .= as0xHex sAddr, "nbytes" .= (as0xHex . unZ80guidanceDisp) disp]

instance FromJSON Z80guidanceMD5Sig where
  parseJSON (Y.String s) = if all (\x -> T.compareLength x 2 == EQ) strChunks && length bytes == 16 && null errs
    then pure $ (Z80guidanceMD5Sig . B.pack . rights) bytes
    else (fail . T.unpack . T.unlines) errs
   where
    strChunks = T.chunksOf 2 s
    bytes     = map convertBytes strChunks
    convertBytes x = fromIntegral <$> convertHex x
    errs = lefts bytes
  parseJSON _ = fail "md5 signature expects a 16 byte hex string, no '0x'."

instance ToJSON Z80guidanceMD5Sig where
  toJSON = Y.String . T.concat . map asHex . B.unpack . unZ80guidanceMD5Sig

parseZ80guidanceAddr :: Y.Value -> Either T.Text Z80guidanceAddr
parseZ80guidanceAddr (Y.String strval) | strval == "$" = pure FromCurPC
                                       | otherwise     = GuidanceAddr <$> convertZ80addr strval
parseZ80guidanceAddr (Y.Number numval) =
  maybe (fail "address exceeds 16-bit unsigned integer range") (pure <$> GuidanceAddr) (S.toBoundedInteger numval)
parseZ80guidanceAddr invalid = fail $ "Invalid Z80 address: " ++ show invalid

parseAddrRange :: Y.Object -> Either T.Text Z80guidanceAddrRange
parseAddrRange attrs = maybe (fail "Missing 'addr' attribute")
                             parseRange
                             (AKM.lookup "addr" attrs)
 where
  endAddrAttr = AKM.lookup "end" attrs
  nBytesAttr  = AKM.lookup "nbytes" attrs <|> AKM.lookup "nBytes" attrs
  parseRange sAddrAttr
    | isJust endAddrAttr
    = AbsRange <$> parseZ80guidanceAddr sAddrAttr <*> parseZ80guidanceAddr (fromJust endAddrAttr)
    | isJust nBytesAttr
    = RelRange <$> parseZ80guidanceAddr sAddrAttr <*>  parseZ80Disp (fromJust nBytesAttr)
    | otherwise
    = fail "Incomplete address range: Missing 'end' or 'nbytes'/'nBytes' attributes."

parseZ80Disp :: Y.Value -> Either T.Text Z80guidanceDisp
parseZ80Disp (Y.String strval) = Z80guidanceDisp <$> convertZ80disp strval
parseZ80Disp (Y.Number numval) =
  maybe (fail "displacement exceeds 16-bit signed integer range") (pure <$> Z80guidanceDisp) (S.toBoundedInteger numval)
parseZ80Disp invalid = fail $ "Invalid Z80 displacement: " ++ show invalid

repackResult :: Either T.Text a -> Parser a
repackResult = either (fail <$> T.unpack) pure

guidanceFields :: JsonOptions
guidanceFields = defaultJsonOptions { jsonFieldName = guidanceJSONMap, jsonTagName = directiveJSONMap }

guidanceJSONMap :: DatatypeName -> FieldName -> JsonFieldName
guidanceJSONMap "Guidance"  "endAddr"  = "end"
guidanceJSONMap "Guidance"  "sections" = "section"
guidanceJSONMap "Guidance"  "origin"   = "origin"
guidanceJSONMap "Directive" fname      = directiveJSONMap fname
guidanceJSONMap dtName      fname      = dtName ++ ":" ++ fname

directiveJSONMap :: ConstructorName -> JsonTagName
directiveJSONMap tag = fromMaybe ("unknown Directive tag: " ++ tag) $ H.lookup tag directiveRenameTable

-- | Translation table mapping 'Directive' constructor names to tags in the YAML/JSON source
directiveRenameTable :: H.HashMap String String
directiveRenameTable = H.fromList
  [ ("MD5Sum"      , "md5")
  , ("SymEquate"   , "equate")
  , ("Comment"     , "comment")
  , ("DoDisasm"    , "disasm")
  , ("GrabBytes"   , "bytes")
  , ("GrabAsciiZ"  , "asciiz")
  , ("GrabAscii"   , "ascii")
  , ("HighBitTable", "highbits")
  , ("JumpTable"   , "jumptable")
  , ("KnownSymbols", "symbols")
  ]

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Convert a text string to a Z80 address. The converted value's range is checked to ensure it is
-- a proper 16-bit quantity.
convertZ80addr :: T.Text -> Either T.Text Z80addr
convertZ80addr str = convertWord16 str >>= checkRange minZ80addr maxZ80addr

-- | Convert a text string to a Z80 displacement. The converted value's range is checked to ensure it is
-- a proper 16-bit quantity.
convertZ80disp :: T.Text -> Either T.Text Z80disp
convertZ80disp str = convertWord16 str >>= checkRange minZ80disp maxZ80disp

-- | Ensure that a converted value (see 'convertZ80addr' and 'convertZ80disp') is properly bounded.
checkRange :: (Integral a) => Int -> Int -> Int -> Either T.Text a
checkRange lower upper val
  | val >= lower && val <= upper = Right (fromIntegral val)
  | otherwise = Left
  $ T.concat ["Value range exceeded (", T.pack (show lower), " <= x <= ", T.pack (show upper), "): ", T.pack (show val)]

convertWord16 :: T.Text -> Either T.Text Int
convertWord16 t | T.isPrefixOf "0x" t = convertHex (T.drop 2 t)
                | T.isPrefixOf "0o" t = convertOctal (T.drop 2 t)
                | T.isPrefixOf "0" t  = convertOctal (T.tail t)
                | otherwise           = convertNum 10 C.isDigit "Invalid decimal constant" t

convertHex, convertOctal :: T.Text -> Either T.Text Int
convertHex = convertNum 16 C.isHexDigit "Invalid hexadecimal constant"
convertOctal = convertNum 8 C.isOctDigit "Invalid octal constant"

minZ80addr :: Int
minZ80addr = fromIntegral (minBound :: Z80addr)

maxZ80addr :: Int
maxZ80addr = fromIntegral (maxBound :: Z80addr)

minZ80disp :: Int
minZ80disp = fromIntegral (minBound :: Z80disp)

maxZ80disp :: Int
maxZ80disp = fromIntegral (maxBound :: Z80disp)

convertNum :: Int -> (Char -> Bool) -> T.Text -> T.Text -> Either T.Text Int
convertNum base digitValid errMsg str = if T.all digitValid str
  then Right $ str2Num str
  else Left $ T.concat [errMsg, ": '", str, T.singleton '\'']
 where
  str2Num = fst . T.mapAccumR digit2num 0 . T.reverse {-str-}
   where
    digit2num v c = (v * base + digitCorrect c, c)
    -- For base 8 and base 10, no correction needed. For hex, this takes care of 'A'-'F'
    digitCorrect c = let i = fromEnum c in (i .&. 0xf) + ((i .&. 0x40) `shiftR` 6) * 9

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

getKnownSymbols :: V.Vector Directive -> H.HashMap T.Text Z80guidanceAddr
getKnownSymbols dirs =
  let knownSymbols (KnownSymbols _)  = True
      knownSymbols _                 = False
      getSymbols (KnownSymbols syms) = syms
      getSymbols _                   = H.empty
  in  maybe H.empty getSymbols (V.find knownSymbols dirs)

invertKnownSymbols :: V.Vector Directive -> H.HashMap Z80addr T.Text
invertKnownSymbols = H.fromList . map flipSymAddr . H.toList . getKnownSymbols {-guidance-}
 where
  flipSymAddr (sym, GuidanceAddr addr)   = (addr, sym)
  flipSymAddr (sym, FromCurPC          ) = error ("'$' used in symbol " ++ show sym)

getMatchingSection :: Guidance -> B.ByteString -> Maybe (V.Vector Directive)
getMatchingSection g md5sum =
  let matchesMD5 dirs = isJust $ V.find
        (\case
          (MD5Sum sig) -> md5sum == unZ80guidanceMD5Sig sig
          _            -> False
        )
        dirs
      filteredSects = H.filter matchesMD5 . sections
      sectKeys      = H.keys $ filteredSects g
  in  if not (null $ filteredSects g) && length sectKeys == 1 then Just (filteredSects g H.! head sectKeys) else Nothing

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Diagnostic functions:
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

tryit :: IO (Either Y.ParseException Guidance)
tryit = Y.decodeFileEither "/tmp/guidance.yaml"

tryit2 :: IO ()
tryit2 = tryit >>= either print (Y.encodeFile "/tmp/out.yaml")
