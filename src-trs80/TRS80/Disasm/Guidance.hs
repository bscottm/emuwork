{-# LANGUAGE LambdaCase #-}

{- | TRS-80 ROM disassembly guidance
-}

module TRS80.Disasm.Guidance
  ( Guidance(..)
  , yamlStringGuidance
  , yamlFileGuidance
  , Directive(..)
  , Z80guidanceAddr(..)
  , Z80guidanceDisp(..)
  , Z80guidanceAddrRange(..)
  , getKnownSymbols
  , invertKnownSymbols
  , getMatchingSection
  , ToJSON(..)
  , FromJSON(..)
  )
where

import           Control.Applicative  ((<|>))

import qualified Data.Aeson.KeyMap    as AKM (lookup, toList)
import           Data.Aeson.Types     ((.:), (.:?), (.=))
import           Data.Bits
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as B
import qualified Data.Char            as C
import           Data.Either          (lefts, rights)
import qualified Data.HashMap.Strict  as H
import           Data.Maybe           (fromJust, fromMaybe, isJust)
import qualified Data.Scientific      as S
import qualified Data.Text            as T
import           Data.Vector          ((!))
import qualified Data.Vector          as V
import           Data.Yaml            (FromJSON (..), Parser, ToJSON (..), object, array)
import qualified Data.Yaml            as Y

-- import           Debug.Trace

import           Machine.Utils                  ( ShowHex(..)
                                                , as0xHex
                                                , as0xHexS
                                                , asHex
                                                )
import           Z80                            ( Z80addr
                                                , Z80disp, z80MinAddr, z80MaxAddr
                                                )

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- There isn't an |instance e ~ Text => MonadFail (Either e)| when Either uses T.Text for Left and
-- we're using OverloadedStrings. Creating that instance results in an orphan.
--
-- Here's the newtype approach to coding around the orphan instance problem 
--
-- Note that there are a few places where unEitherText has to be invoked to get the Either object
-- back for 

newtype EitherText a = EitherText { unEitherText :: Either T.Text a }

instance Functor EitherText where
  -- fmap :: (a -> b) -> EitherText a -> EitherText b
  fmap f (EitherText e) = EitherText $ fmap f e

instance Applicative EitherText where
  -- pure :: a -> EitherText a
  pure = EitherText . Right
  -- (<*>) :: EitherText (a -> b) -> EitherText a -> EitherText b
  (EitherText f) <*> (EitherText a) = EitherText $ f <*> a

instance Monad EitherText where
  -- (>>=) :: EitherText a -> (a -> EitherText b) -> EitherText b
  (EitherText a) >>= f = EitherText $ a >>= (unEitherText . f)

instance MonadFail EitherText where
  -- fail :: String -> EitherText a
  fail = EitherText . Left . T.pack

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

parseZ80guidanceAddr :: Y.Value -> EitherText Z80guidanceAddr
parseZ80guidanceAddr (Y.String strval) | strval == "$" = pure FromCurPC
                                       | otherwise     = GuidanceAddr <$> convertZ80addr strval
parseZ80guidanceAddr (Y.Number numval) =
  maybe (fail "address exceeds 16-bit unsigned integer range") (pure <$> GuidanceAddr) (S.toBoundedInteger numval)
parseZ80guidanceAddr invalid = fail $ "Invalid Z80 address: " ++ show invalid

parseAddrRange :: Y.Object -> EitherText Z80guidanceAddrRange
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

parseZ80Disp :: Y.Value -> EitherText Z80guidanceDisp
parseZ80Disp (Y.String strval) = Z80guidanceDisp <$> convertZ80disp strval
parseZ80Disp (Y.Number numval) =
  maybe (fail "displacement exceeds 16-bit signed integer range") (pure <$> Z80guidanceDisp) (S.toBoundedInteger numval)
parseZ80Disp invalid = fail $ "Invalid Z80 displacement: " ++ show invalid

repackResult :: EitherText a -> Parser a
repackResult = either (fail <$> T.unpack) pure . unEitherText

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Convert a text string to a Z80 address. The converted value's range is checked to ensure it is
-- a proper 16-bit quantity.
convertZ80addr :: T.Text -> EitherText Z80addr
convertZ80addr str = convertWord16 str >>= checkRange minZ80addr maxZ80addr

-- | Convert a text string to a Z80 displacement. The converted value's range is checked to ensure it is
-- a proper 16-bit quantity.
convertZ80disp :: T.Text -> EitherText Z80disp
convertZ80disp str = convertWord16 str >>= checkRange minZ80disp maxZ80disp

-- | Ensure that a converted value is properly bounded (see 'convertZ80addr' and 'convertZ80disp'.)
checkRange :: (Integral a) => Int -> Int -> Int -> EitherText a
checkRange lower upper val
  | val >= lower && val <= upper
  = pure . fromIntegral $ val
  | otherwise
  -- You'd like to use fail here, but fail :: String -> ...
  = EitherText . Left . T.pack . concat $ ["Value range exceeded ("
                                          , show lower
                                          , " <= x <= "
                                          , show upper
                                          , "): "
                                          , show val
                                          ]

convertWord16 :: T.Text -> EitherText Int
convertWord16 t | T.isPrefixOf "0x" t = convertHex (T.drop 2 t)
                | T.isPrefixOf "0o" t = convertOctal (T.drop 2 t)
                | T.isPrefixOf "0" t  = convertOctal (T.tail t)
                | otherwise           = convertNum 10 C.isDigit "Invalid decimal constant" t

convertHex, convertOctal :: T.Text -> EitherText Int
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

convertNum :: Int -> (Char -> Bool) -> T.Text -> T.Text -> EitherText Int
convertNum base digitValid errMsg str
  | T.all digitValid str
  = pure . str2Num $ str
  | otherwise
  = EitherText . Left . T.concat $ [errMsg, ": '", str, T.singleton '\'']
 where
  str2Num = fst . T.mapAccumR digit2num 0 . T.reverse {-str-}
   where
    digit2num v c = (v * base + digitCorrect c, c)
    -- For base 8 and base 10, no correction needed. For hex, this takes care of 'A'-'F'
    digitCorrect c = let i = fromEnum c in (i .&. 0xf) + ((i .&. 0x40) `shiftR` 6) * 9

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

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Wrapper type for 'Z80disp' so that we can write our own 'Value' parser
newtype Z80guidanceDisp =
  Z80guidanceDisp
  {
    unZ80guidanceDisp :: Z80disp
  } deriving (Eq, Ord)

instance Show Z80guidanceDisp where
  show = as0xHexS . unZ80guidanceDisp

instance FromJSON Z80guidanceDisp where
  parseJSON = repackResult . parseZ80Disp

instance ToJSON Z80guidanceDisp where
  toJSON = Y.String . as0xHex . unZ80guidanceDisp

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Wrapper type for address ranges pairs. This accepts "[start address, end address]" or "start", "end"/"nbytes", "nBytes"
-- attribute/value pairs.
data Z80guidanceAddrRange =
    AbsRange Z80guidanceAddr Z80guidanceAddr
  -- ^ Absolute range, where the end address is absolute
  | RelRange Z80guidanceAddr Z80guidanceDisp
  -- ^ Relative range, where the end address is a displacement (i.e., relative)
  deriving (Eq)

instance FromJSON Z80guidanceAddr where
  parseJSON = repackResult . parseZ80guidanceAddr

instance ToJSON Z80guidanceAddr where
  toJSON FromCurPC           = Y.String "$"
  toJSON (GuidanceAddr addr) = (Y.String . as0xHex) addr

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
instance Show Z80guidanceAddrRange where
  show (AbsRange sAddr eAddr) = "AbsRange(" ++ as0xHexS sAddr ++ ", " ++ as0xHexS eAddr ++ ")"
  show (RelRange sAddr disp ) = "RelRange(" ++ as0xHexS sAddr ++ ", " ++ (as0xHexS . unZ80guidanceDisp) disp ++ ")"

instance FromJSON Z80guidanceAddrRange where
  parseJSON (Y.Array vals)
    | V.length vals /= 2 = fail "Address range must have 2 elements, [start, end]"
    | otherwise          = repackResult $ AbsRange <$> parseZ80guidanceAddr (vals ! 1) <*> parseZ80guidanceAddr (vals ! 0)
  parseJSON (Y.Object attrs) = repackResult $ parseAddrRange attrs
  parseJSON _                = fail "Address range should have 'start' and 'end' or 'nbytes'/'nBytes' attributes"

instance ToJSON Z80guidanceAddrRange where
  toJSON (AbsRange sAddr eAddr) = object ["addr" .= toJSON sAddr, "end" .= toJSON eAddr]
  toJSON (RelRange sAddr disp ) = object ["addr" .= toJSON sAddr, "nbytes" .= (toJSON . unZ80guidanceDisp $ disp)]

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Wrapper type for MD5 signatures
newtype Z80guidanceMD5Sig =
  Z80guidanceMD5Sig {
    unZ80guidanceMD5Sig :: B.ByteString
  } deriving (Eq, Show)

instance FromJSON Z80guidanceMD5Sig where
  parseJSON (Y.String s) = if all (\x -> T.compareLength x 2 == EQ) strChunks && length bytes == 16 && null errs
    then pure . Z80guidanceMD5Sig . B.pack . rights . unBytes $ bytes
    else fail . T.unpack . T.unlines $ errs
   where
    strChunks = T.chunksOf 2 s
    bytes     = map convertBytes strChunks
    convertBytes x = fromIntegral <$> convertHex x
    errs = lefts . unBytes $ bytes
    unBytes = map unEitherText
  parseJSON _ = fail "md5 signature expects a 16 byte hex string, no '0x'."

instance ToJSON Z80guidanceMD5Sig where
  toJSON = Y.String . T.concat . map asHex . B.unpack . unZ80guidanceMD5Sig

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Disassembly directives that occur within a "section"
data Directive where
  MD5Sum       :: Z80guidanceMD5Sig -> Directive
  SymEquate    :: T.Text -> Z80guidanceAddr -> Directive
  Comment      :: T.Text -> Directive
  DoDisasm     :: Z80guidanceAddrRange -> Directive
  GrabBytes    :: Z80guidanceAddrRange -> Directive
  GrabAsciiZ   :: Z80guidanceAddr -> Directive
  GrabAscii    :: Z80guidanceAddrRange -> Directive
  HighBitTable :: Z80guidanceAddrRange -> Directive
  JumpTable    :: Z80guidanceAddrRange -> Directive
  KnownSymbols :: (H.HashMap T.Text Z80guidanceAddr) -> Directive
  deriving (Eq, Show)

instance FromJSON Directive where
  parseJSON (Y.Object thing)
    {- | trace ("FromJSON Directive " ++ show thing) False
    = undefined
    | otherwise -}
    = mkDirective . head . AKM.toList $ thing
    where
      mkDirective (label, content) =
        case label of
          "ascii"     -> GrabAscii <$> parseJSON content
          "asciiz"    -> GrabAsciiZ <$> parseJSON content
          "bytes"     -> GrabBytes <$> parseJSON content
          "comment"   -> Comment <$> parseJSON content
          "disasm"    -> DoDisasm <$> parseJSON content
          "equate"    -> parseEquate content
          "highbits"  -> HighBitTable <$> parseJSON content
          "jumptable" -> JumpTable <$> parseJSON content
          "md5"       -> MD5Sum <$> parseJSON content
          "symbols"   -> KnownSymbols <$> parseJSON content
          _           -> fail ("bad directive: " ++ show label)

      parseEquate (Y.Array a) =
        SymEquate
          <$> parseSymEquateName (a ! 0)
          <*> parseJSON (a ! 1)
      parseEquate (Y.Object o) = 
        SymEquate
          <$> (parseSymEquateName =<< o .: "name")
          <*> o .: "value"
      parseEquate obj = fail ("invalid equate: " ++ show obj)

      parseSymEquateName (Y.String name)
        | goodFirstChar . T.head $ name
        = pure name
        | otherwise
        = fail ("invalid symbolic equate name: " ++ show name)
        where
          goodFirstChar c = C.isAlpha c || c == '_'

      parseSymEquateName badsym = fail ("invalid symbolic equate name: " ++ show badsym)
  
  parseJSON thing = fail ("invalid directive: " ++ show thing)

instance ToJSON Directive where
  toJSON (MD5Sum sig) = object [ "md5" .= sig ]
  toJSON (SymEquate name value) = object [ "equate" .=  array [ toJSON name, toJSON value ]]
  toJSON (Comment cmnt) = object [ "comment" .= cmnt ]
  toJSON (DoDisasm range) = object [ "disasm" .= range ]
  toJSON (GrabBytes range) = object [ "bytes" .= range ]
  toJSON (GrabAsciiZ addr) = object [ "asciiz" .= addr ]
  toJSON (GrabAscii range) = object [ "ascii" .= range ]
  toJSON (HighBitTable range) = object [ "highbits" .= range ]
  toJSON (JumpTable range) = object [ "jumptable" .= range ]
  toJSON (KnownSymbols symtab) = object [ "symbols" .= symtab ]

data YAMLGuidance where
  YAMLGuidance ::
    { yamlOrigin   :: Maybe Z80guidanceAddr
      -- Disassembly origin address (where to start disassembling)
    , yamlEndAddr  :: Maybe Z80guidanceAddr
      -- End disassembly address
    , yamlSections :: Maybe (H.HashMap T.Text [Directive])
      -- Map of section names to a list of directives to apply
    } -> YAMLGuidance
  deriving (Eq, Show)

instance FromJSON YAMLGuidance where
  parseJSON = Y.withObject "YAMLGuidance" (\v ->
    YAMLGuidance
      <$> (v .:? "origin" >>= checkNotFromCurPC "origin")
      <*> (v .:? "end" >>= checkNotFromCurPC "end")
      <*> v .:? "section")
    where
      checkNotFromCurPC attr (Just FromCurPC) = fail ("Cannot set " ++ T.unpack attr ++ " to current PC ($)")
      checkNotFromCurPC _    thing            = pure thing


instance ToJSON YAMLGuidance where
  toJSON guidance = object [ "origin" .= yamlOrigin guidance
                           , "end" .= yamlEndAddr guidance
                           , "section" .= yamlSections guidance
                           ]

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- | Disassembler guidance: When to disassemble, when to dump bytes, ... basically guidance to the drive
-- the disassembly process (could be made more generic as part of a 'Machine' module.)
data Guidance where
  Guidance ::
    { origin   :: Z80guidanceAddr
      -- Disassembly origin address (where to start disassembling)
    , endAddr  :: Z80guidanceAddr
      -- End disassembly address
    , sections :: H.HashMap T.Text (V.Vector Directive)
      -- Map of section names to a list of directives to apply
    } -> Guidance
  deriving (Eq, Show)

instance FromJSON Guidance where
  parseJSON = fmap convert <$> parseJSON
    where
      -- Converts YAMLGuidance to Guidance. :-)
      convert yaml = Guidance {
        origin = fromMaybe (GuidanceAddr z80MinAddr) (yamlOrigin yaml),
        endAddr = fromMaybe (GuidanceAddr z80MaxAddr) (yamlEndAddr yaml),
        sections = H.map V.fromList $ fromMaybe H.empty (yamlSections yaml)
      }

instance ToJSON Guidance where
  toJSON guidance = Y.object [
      "origin" .= origin guidance
    , "end" .= endAddr guidance
    , "section" .= sections guidance
    ]

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
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

-- Data.Digest.Pure.MD5 uses lazy byte strings, need to upconvert to strict byte strings
-- for YAML -> S.concat . B.toChunks
yamlStringGuidance :: B.ByteString -> Either Y.ParseException Guidance
yamlStringGuidance = Y.decodeEither' . S.concat . B.toChunks

yamlFileGuidance :: FilePath -> IO (Either Y.ParseException Guidance)
yamlFileGuidance = Y.decodeFileEither
