{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Disasm.YamlTest where

import           Text.RawString.QQ
import           Data.ByteString (ByteString)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Disasm.Guidance


t1 :: ByteString
t1 = [r|
- origin: 0x0
- comment: Restart vector redirections. These are 'JP' instructions
- comment: |
    multiline comment
    comment line 2
|]

test1 :: IO ()
test1 =
  do
    putStrLn "t1 Input:"
    putStrLn "~~~~"
    putStrLn $ (T.unpack . decodeUtf8) t1
    putStrLn "~~~~"

    case Y.decodeEither' t1 :: Either Y.ParseException [Guidance] of
      Left  err -> putStrLn ("Decode failed: " ++ (show err))
      Right res ->
        do
          putStrLn (show res)
          putStrLn "Succeeded."


t2 :: ByteString
t2 = [r|- origin: 0x0
- comment: |
    multiline comment
    comment line 2
- equate:
    name: RST08VEC
    value: 0x4000
- comment: RST10 vector - this is a JP instruction
- equate:
    name: RST10VEC
    value: 0x4003
|]

test2 :: IO ()
test2 =
  do
    putStrLn "t2 Input:"
    putStrLn "~~~~"
    putStrLn $ (T.unpack . decodeUtf8) t2
    putStrLn "~~~~"

    case Y.decodeEither' t2 :: Either Y.ParseException [Guidance] of
      Left  err -> putStrLn ("Decode failed: " ++ (show err))
      Right res ->
        do
          putStrLn (show res)
          putStrLn "Succeeded."