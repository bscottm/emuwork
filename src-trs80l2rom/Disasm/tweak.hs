{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Disasm.Tweak where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Text.RawString.QQ
import qualified Data.Yaml as Y

config :: ByteString
config = [r|
- origin: 0
- comment: Restart vector redirections. These are 'JP' instructions
|]

foo :: IO ()
foo = print (Y.decode config :: Maybe [])
