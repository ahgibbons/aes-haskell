module Types where

import qualified Data.ByteString as BS

type IV = BS.ByteString

data AES = AES128 | AES192 | AES256 deriving (Show,Eq)
type PlainText = BS.ByteString
type Key = BS.ByteString
type CipherText = BS.ByteString

