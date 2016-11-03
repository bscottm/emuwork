{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TRS80.Disasm.Guidance
  ( Guidance(..)
  , Directive(..)
  , getKnownSymbols
  , getMatchingSection
  , ToJSON(..)
  , FromJSON(..)
  ) where

import qualified Data.Aeson.Types as AT
import           Data.Bits
import qualified Data.ByteString.Lazy as BCL
import qualified Data.Char as C
import           Data.Either
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml (FromJSON(..), ToJSON(..), (.=))
import qualified Data.Yaml as Y
-- import           Debug.Trace

import           Machine.Utils (as0xHex, asHex)
import           Z80 (Z80addr, Z80disp)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Disassembler "guidance": When to disassemble, when to dump bytes, ... basically guidance to the drive
-- the disassembly process (could be made more generic as part of a 'Machine' module.)
data Guidance where
  Guidance ::
    { origin      :: Z80addr
                  -- Disassembly origin address (where to start disassembling)
    , haveOrigin  :: Bool
                  -- Avoids using Maybe to test if the origin was specified
    , endAddr     :: Z80addr
                  -- End disassembly address
    , haveEndAddr :: Bool
    , sections    :: H.HashMap T.Text (V.Vector Directive)
                  -- Map of section names to a list of directives to apply
    } -> Guidance
  deriving (Eq, Show)

instance ToJSON Guidance where
  toJSON (Guidance org _haveOrg ea _haveEA sects)  =
    Y.Object $ H.insert "end" (Y.String $ as0xHex ea) $
               H.insert "origin" (Y.String $ as0xHex org) $
               H.map (Y.Array . V.map toJSON) sects

instance FromJSON Guidance where
  parseJSON (Y.Object o) =
    {- trace ("FromJSON object: " ++ (show o)) -}
    repackage $ Foldable.foldl collectGuidance
                               (return Guidance { origin      = 0x0
                                                , haveOrigin  = False
                                                , endAddr     = 0x0
                                                , haveEndAddr = False
                                                , sections    = H.empty
                                                })
                               (H.toList o)
    where
      collectGuidance :: Either T.Text Guidance
                      -> (T.Text, Y.Value)
                      -> Either T.Text Guidance
      collectGuidance guidance ("origin", orgAddr)   = either (fail . T.unpack) (mkOrigin orgAddr) guidance
      collectGuidance guidance ("end", ea)           = either (fail . T.unpack) (mkEndAddr ea) guidance
      collectGuidance guidance (section, directives) = either (fail . T.unpack) (mkSection section directives) guidance

      repackage = either (fail . T.unpack) return

  {- Catchall -}
  parseJSON invalid = AT.typeMismatch "Guidance" invalid

mkOrigin :: AT.Value
         -> Guidance
         -> Either T.Text Guidance
mkOrigin (Y.String s) g = (\org -> g { origin = org, haveOrigin = True }) <$> convertWord16 s
mkOrigin (Y.Number n) g = maybe (outOfRange minZ80addr maxZ80addr ((T.pack . show) n))
                               (\n' -> return $ g { origin = n', haveOrigin = True })
                               (S.toBoundedInteger n)
mkOrigin something _g   = Left $ T.concat ["origin expected a numeric value, got '"
                                          , T.pack (show something)
                                          , singleQuote
                                          ]

mkEndAddr :: AT.Value
          -> Guidance
          -> Either T.Text Guidance
mkEndAddr (Y.String s) g = (\ea -> g { endAddr = ea, haveEndAddr = True }) <$> convertWord16 s
mkEndAddr (Y.Number n) g = maybe (outOfRange minZ80addr maxZ80addr ((T.pack . show) n))
                                 (\n' -> Right $ g { endAddr =  n', haveEndAddr = True})
                                 (S.toBoundedInteger n)
mkEndAddr something _g   = Left $ T.concat ["end expected a numeric value, got '"
                                           , T.pack (show something)
                                           , singleQuote
                                           ]

mkSection :: T.Text
          -> Y.Value
          -> Guidance
          -> Either T.Text Guidance
mkSection sectName (Y.Array directives) g =
  let dirs   = V.toList $ V.map parseDirective directives
      errs   = lefts dirs
      result = (V.fromList . rights) dirs
  in  {- | trace ("directives = " ++ (show directives)) -}
    if null errs
    then Right $ g { sections = H.insert sectName result (sections g) }
    else Left (T.unlines errs)
mkSection sectName _directives _ = fail (T.unpack $ T.concat [ "Section '"
                                                             , sectName
                                                             , singleQuote
                                                             , " expects a directive list."
                                                             ])

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

data Directive where
  MD5Sum         :: BCL.ByteString
                 -- MD5 signature: Conditionally apply the directives iff the section's signature matches
                 -> Directive
  SymEquate      :: T.Text
                 -- Symbolic name
                 -> Z80addr
                 -- Value associated iwth with the symbolic name
                 -> Directive
  Comment        :: T.Text
                 -- Comment text
                 -> Directive
  DoDisasm       :: Z80addr
                 -- Start disassembly address
                 -> Z80disp
                 -- Number of bytes to disassemble
                 -> Directive
  GrabBytes      :: Z80addr
                 -- Start of range
                 -> Z80disp
                 -- Number of bytes to grab
                 -> Directive
  GrabAsciiZ     :: Z80addr
                    -- Start address to start grabbing 0-terminated ASCII string
                 -> Directive
  GrabAscii      :: Z80addr
                 -- Start of range
                 -> Z80disp
                 -- Number of bytes to grab
                 -> Directive
  HighBitTable   :: Z80addr
                 -- Start of table
                 -> Z80disp
                 -- Table length
                 -> Directive
  JumpTable      :: Z80addr
                 -- Jump table start
                 -> Z80disp
                 -- Jump table length
                 -> Directive
  KnownSymbols   :: H.HashMap Z80addr T.Text
                    -- Mapping between addresses and symbols, more user friendly output
                 -> Directive
  deriving (Eq, Show)

instance ToJSON Directive where
  toJSON (MD5Sum md5sum)          = Y.object ["md5" .= md5AsText md5sum]
  toJSON (SymEquate sym addr)     = Y.object ["equate" .= Y.object [ "name" .= sym
                                                                   , "value" .= as0xHex addr]
                                             ]
  toJSON (Comment comment)        = Y.object ["comment" .= comment]
  toJSON (DoDisasm addr disp)     = Y.object ["disasm" .= Y.object [ "addr" .= as0xHex addr
                                                                   , "nbytes" .= as0xHex disp]
                                             ]
  toJSON (GrabBytes addr disp)    = Y.object ["bytes" .= Y.object [ "addr" .= as0xHex addr
                                                                  , "nbytes" .= as0xHex disp
                                                                  ]
                                             ]
  toJSON (GrabAsciiZ addr)        = Y.object ["asciiz" .= addr]
  toJSON (GrabAscii addr disp)    = Y.object ["ascii" .= Y.object [ "addr" .= as0xHex addr
                                                                  , "nbytes" .= as0xHex disp
                                                                  ]
                                             ]
  toJSON (HighBitTable addr disp) = Y.object ["highbits" .= Y.object [ "addr" .= as0xHex addr
                                                                     , "nbytes" .= as0xHex disp
                                                                     ]
                                             ]
  toJSON (JumpTable addr disp)    = Y.object ["jumptable" .= Y.object ["addr" .= as0xHex addr
                                                                      , "nbytes" .= as0xHex disp
                                                                      ]
                                             ]
  toJSON (KnownSymbols _syms)     = undefined

{-
  toEncoding (SetOrigin addr) = pairs ("addr" .= addr)
  toEncoding (SymEquate sym addr) = pairs ("equate" .= object [sym .= addr])
  toEncoding (Comment comment) = pairs ("comment" .= comment)
  toEncoding (DoDisasm addr disp) = pairs ("disasm" .= object ["start" .= addr, "nbytes" .= disp])
  toEncoding (GrabBytes addr disp) = pairs ("bytes" .= object ["addr" .= addr, "nbytes" .= disp])
  toEncoding (GrabAsciiZ addr) = pairs ("asciiz" .= addr)
  toEncoding (GrabAscii addr disp) = pairs ("ascii" .= object ["addr" .= addr, "len" .= disp])
  toEncoding (HighBitTable addr disp) = pairs ("highbits" .= object ["addr" .= addr, "nbytes" .= disp])
  toEncoding (JumpTable addr disp) = pairs ("jumptable" .= object ["addr" .= addr, "nbytes" .= disp])
-}

md5AsText :: BCL.ByteString -> T.Text
md5AsText = BCL.foldl (\accum digit -> T.append accum $ asHex digit) T.empty
  
instance FromJSON Directive where
  parseJSON val@(Y.Object _) = either (fail . T.unpack) return (parseDirective val)
  parseJSON invalid          = AT.typeMismatch "Directive" invalid

parseDirective :: Y.Value
                -> Either T.Text Directive
parseDirective (Y.Object o)
    | (v, exists) <- probe "comment" o
    , exists
    -- , trace ("comment: v = " ++ (show v)) True
    = mkComment v
    | (v, exists) <- probe "equate" o
    -- , trace ("comment: v = " ++ (show v)) True
    , exists
    = mkEquate v
    | (v, exists) <- probe "disasm" o
    -- , trace ("disasm: v = " ++ (show v)) True
    , exists
    = mkDisasm v
    | (v, exists) <- probe "bytes" o
    -- , trace ("bytes: v = " ++ (show v)) True
    , exists
    = mkGrabBytes v
    | (v, exists) <- probe "asciiz" o
    -- , trace ("asciiz: v = " ++ (show v)) True
    , exists
    = mkAsciiZ v
    | (v, exists) <- probe "ascii" o
    -- , trace ("ascii: v = " ++ (show v)) True
    , exists
    = mkGrabAscii v
    | (v, exists) <- probe "highbits" o
    -- , trace ("highbits: v = " ++ (show v)) True
    , exists
    = mkHighBitTable v
    | (v, exists) <- probe "jumptable" o
    -- , trace ("jumptable: v = " ++ (show v)) True
    , exists
    = mkJumpTable v
    | (v, exists) <- probe "symbols" o
    {-  , trace ("symbols: v = " ++ (show v)) True -}
    , exists
    = mkKnownSymbols v
    | (v, exists) <- probe "md5" o
    {-  , trace ("md5: v = " ++ (show v)) True -}
    , exists
    = mkMD5Sum v
    | otherwise
    = fail ("Valid directive expected, got: " ++ show o)
    where
      probe k h   = let v = H.lookup k h
                    in  (v, isJust v)

parseDirective _invalid = Left "Expected a directive (key: value) pair."

mkComment :: Maybe AT.Value -> Either T.Text Directive
mkComment (Just (Y.String s)) = Right $ Comment s
mkComment _                   = Left "Comment guidance expects a string."

mkEquate :: Maybe AT.Value -> Either T.Text Directive
mkEquate (Just (Y.Object o'))  =
  let symname = H.lookup "name" o'
      symval  = H.lookup "value" o'
  in case symname of
       Just (AT.String symname') -> if validSymName symname'
                                    then case symval of
                                           Just (AT.String symval') -> SymEquate symname' <$> convertWord16 symval'
                                           Just (AT.Number symval') -> maybe (outOfRange minZ80addr maxZ80addr
                                                                                        ((T.pack . show ) symval'))
                                                                             (Right . SymEquate symname')
                                                                             (S.toBoundedInteger symval')
                                           Just something           -> Left $ T.concat ["String expected for equate value: '"
                                                                                       , T.pack (show something)
                                                                                       , singleQuote
                                                                                       ]
                                           Nothing                  -> Left "Missing symbol value in equate"
                                    else Left $ T.concat ["Invalid equate name (max 15 chars, '[A-Z]$_@' first char)': '"
                                                         , symname'
                                                         , singleQuote
                                                         ]
       Just something            -> Left $ T.concat ["String expected for equate symbol name: '"
                                                    , T.pack (show something)
                                                    , singleQuote
                                                    ]
       Nothing                   -> Left "Missing symbol name in equate"
mkEquate _                    = Left "equate directive expects a name and a value (name, value dict.)"

mkAsciiZ :: Maybe AT.Value -> Either T.Text Directive
mkAsciiZ (Just (Y.String addr)) = GrabAsciiZ <$> convertWord16 addr
mkAsciiZ (Just (Y.Number addr)) = maybe (outOfRange minZ80addr maxZ80addr addr) (Right . GrabAsciiZ) (S.toBoundedInteger addr)
mkAsciiZ _                      = Left "asciiz expects an address"

mkDisasm :: Maybe AT.Value -> Either T.Text Directive
mkDisasm = rdStartAndLength DoDisasm

mkGrabBytes :: Maybe AT.Value -> Either T.Text Directive
mkGrabBytes = rdStartAndLength GrabBytes

mkGrabAscii :: Maybe AT.Value -> Either T.Text Directive
mkGrabAscii = rdStartAndLength GrabAscii

mkHighBitTable :: Maybe AT.Value -> Either T.Text Directive
mkHighBitTable = rdStartAndLength HighBitTable

mkJumpTable :: Maybe AT.Value -> Either T.Text Directive
mkJumpTable = rdStartAndLength JumpTable

mkKnownSymbols :: Maybe AT.Value -> Either T.Text Directive
mkKnownSymbols (Just (Y.Object syms)) =
  let syms'                      = H.toList $ H.mapWithKey symConvert syms

      symConvert _k (Y.String s) = convertWord16 s
      symConvert _k (Y.Number n) = maybe (outOfRange minZ80addr maxZ80addr n) Right (S.toBoundedInteger n)
      symConvert k  something    = Left $ T.concat [ "Expected an address in '"
                                                   , k
                                                   , singleQuote
                                                   , " mapping, got "
                                                   , (T.pack . show) something
                                                   ]

      cvtErrs                    = [ getLeft (snd x)  | x <- syms', isLeft (snd x) ]
      getLeft (Left x)           = x
      getLeft (Right _)          = error "Should only have Left elements in this list."

      goodElts                   = [ (getRight (snd x), fst x) | x <- syms', isRight (snd x) ]
      getRight (Right x)         = x
      getRight (Left _)          = error "Should only have Right elements in this list."
  in  if   null cvtErrs
      then Right $ KnownSymbols $ H.fromList goodElts
      else Left $ T.unlines cvtErrs
mkKnownSymbols _ = Left "symbols expects a map of symbol names to addresses"

mkMD5Sum ::  Maybe AT.Value -> Either T.Text Directive
mkMD5Sum (Just (Y.String s)) =
  let strBytes = T.chunksOf 2 s
      bytes    = map convertHex strBytes
      errs     = lefts bytes
  in  if all (\x -> T.compareLength x 2 == EQ) strBytes && length bytes == 16 && null errs
      then (Right . MD5Sum . BCL.pack . rights) bytes
      else Left (T.unlines errs)
mkMD5Sum _                   = Left "md5 expects a 16 byte hex string (no '0x')"
  
convertWord16 :: forall a. (Integral a, Bounded a) => T.Text -> Either T.Text a
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

convertHex :: forall a. (Integral a, Bounded a) => T.Text -> Either T.Text a
convertHex t =
  let val = fst $ T.mapAccumR (\v c -> (v * 16 + hexDigit c, c)) 0 $ T.reverse t
      hexDigit c = let i = fromEnum c
                   in  (i .&. 0xf) + ((i .&. 0x40) `shiftR` 6) * 9
      vMin = fromIntegral (minBound :: a)
      vMax = fromIntegral (maxBound :: a)
  in if T.all (\c -> let c' = fromEnum c
                             in (c' >= fromEnum '0' && (c' <= fromEnum '9')) ||
                                (c' >= fromEnum 'a' && (c' <= fromEnum 'f')) ||
                                (c' >= fromEnum 'A' && (c' <= fromEnum 'F'))) t
      then if val >= vMin && val <= vMax           then Right $ fromIntegral val
           else outOfRange vMin vMax t
     else Left $ T.concat ["Invalid hexadecimal constant: '", t, singleQuote]

convertOctal :: forall a. (Integral a, Bounded a) => T.Text -> Either T.Text a
convertOctal octstr =
  let val = fst $ T.mapAccumR (\v c -> (v * 8 + (fromEnum c .&. 0xf), c)) 0 (T.reverse octstr)
      validOctal = T.all (\c -> let c' = fromEnum c
                                in  (c' >= fromEnum '0' && c' <= fromEnum '7'))
      vMin = fromIntegral (minBound :: a)
      vMax = fromIntegral (maxBound :: a)
  in  if validOctal octstr
      then if val >= vMin && val <= vMax
           then Right $ fromIntegral val
           else outOfRange minBound maxBound octstr
      else Left $ T.concat ["Invalid octal constant: '", octstr, singleQuote]

convertDecimal :: forall a. (Integral a, Bounded a) => T.Text -> Either T.Text a
convertDecimal str =
  let val = fst $ T.mapAccumR (\v c -> (v * 10 + (fromEnum c .&. 0xf), c)) 0 (T.reverse str)
      vMin = fromIntegral (minBound :: a)
      vMax = fromIntegral (maxBound :: a)
  in  if T.all C.isDigit str
      then if val >= vMin && val <= vMax
           then Right (fromIntegral val)
           else outOfRange minBound maxBound str
      else Left $ T.concat ["Invalid constant: '", str, singleQuote]

validSymName :: T.Text -> Bool
validSymName sym = let validChar x = (C.isLetter x || x == '$' || x == '_' || x == '@')
                   in  (validChar . T.head) sym
                       && T.compareLength sym 15 /= GT
                       && T.all (\x -> validChar x || C.isDigit x || x == '?') (T.tail sym)

outOfRange :: forall b a. (Show a) => Int -> Int -> a -> Either T.Text b
outOfRange minRange maxRange thing = Left $ T.concat ["Value range exceeded ("
                                                     , T.pack (show minRange)
                                                     , " <= x <= "
                                                     , T.pack (show maxRange)
                                                     , "): "
                                                     , T.pack (show thing)
                                                     ]

singleQuote :: T.Text
singleQuote = T.singleton '\''

rdStartAndLength :: (Z80addr -> Z80disp -> Directive) -> Maybe AT.Value -> Either T.Text Directive
rdStartAndLength tyCon (Just (Y.Object o)) =
  let endAddr = H.lookup "end" o
      nBytes  = let nb' = H.lookup "nBytes" o
                    in case nb' of
                         (Just _) -> nb'
                         Nothing  -> H.lookup "nbytes" o

      rdLength :: Maybe AT.Value -> Maybe AT.Value -> Either T.Text Z80addr -> Either T.Text Directive
      -- Pass errors through... quickly.
      rdLength _                    _                    (Left err)   = Left err
      rdLength (Just _)             (Just _)             _            = Left "Only one of 'end' or 'nBytes' can be specified."
      rdLength (Just (Y.String ea)) Nothing              (Right sa)   =
        either (\err  -> Left err)
               (\ea'  -> Right $ tyCon sa (fromIntegral (ea' - sa)))
               (convertWord16 ea)
      rdLength (Just (Y.Number ea)) Nothing              (Right sa)   =
        case (S.toBoundedInteger ea) of
          Just (ea' :: Z80addr) -> Right $ tyCon sa (fromIntegral (ea' - sa))
          Nothing -> outOfRange minZ80addr maxZ80addr ea
      rdLength (Just _)             Nothing              _            =
        Left "End address is not a number."
      rdLength Nothing              (Just (Y.String nb)) (Right sa)   =
        either (\err  -> Left err)
               (\nb' -> Right $ tyCon sa nb')
               (convertWord16 nb)
      rdLength Nothing              (Just (Y.Number nb)) (Right sa)   =
        case (S.toBoundedInteger nb) of
          Just (nb' :: Z80disp) -> Right $ tyCon sa nb'
          Nothing -> outOfRange minZ80disp maxZ80disp nb
      rdLength Nothing              (Just _)             _            =
        Left "Number of bytes ('nBytes') is an invalid number."
      rdLength Nothing              Nothing              _            =
        Left $ T.pack "End address or number of btyes ('end'/'nBytes') missing."
  in  case H.lookup "addr" o of
        Just (Y.String startAddr) -> rdLength endAddr nBytes $ convertWord16 startAddr
        Just (Y.Number startAddr) -> case S.toBoundedInteger startAddr of
                                       (Just (sa :: Z80addr)) -> rdLength endAddr nBytes (Right sa)
                                       Nothing                -> outOfRange minZ80addr maxZ80addr startAddr
        Just _something           -> Left "Invalid start address ('addr')."
        Nothing                   -> Left "start address ('addr') key required."

rdStartAndLength _tyCon _anything = Left "Expected a dictionary with 'start' and 'end'/'nBytes'"

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

getKnownSymbols :: V.Vector Directive
                -> Maybe (H.HashMap Z80addr T.Text)
getKnownSymbols dirs = let knownSymbols (KnownSymbols _) = True
                           knownSymbols _                = False
                       in  case V.find knownSymbols dirs of
                             Just (KnownSymbols syms) -> Just syms
                             _                        -> Nothing

getMatchingSection :: Guidance
                   -> BCL.ByteString
                   -> Maybe (V.Vector Directive)
getMatchingSection g md5sum =
  let matchesMD5 dirs = isJust $ V.find (\d -> case d of
                                                 (MD5Sum md5) -> md5sum == md5
                                                 _            -> False) dirs
      filteredSects   = H.filter matchesMD5 (sections g)
      sectKeys        = H.keys filteredSects
  in  if not (null filteredSects) && length sectKeys == 1
      then Just (filteredSects H.! (head sectKeys))
      else Nothing