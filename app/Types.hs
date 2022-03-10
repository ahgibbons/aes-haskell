module Types where
import qualified Data.ByteString as BS

nb = 4 :: Int
nk_128 = 4 :: Int
nk_192 = 6 :: Int
nk_256 = 8 :: Int
nr_128 = 10 :: Int
nr_192 = 12 :: Int
nr_256 = 14 :: Int



type IV = BS.ByteString

data AES = AES128 | AES192 | AES256 deriving (Show,Eq)
data AESKey = Key128 Key | Key192 Key | Key256 Key deriving (Show, Eq)
makeKey :: AES -> Key -> Either String AESKey
makeKey AES128 k
    | BS.length k == nk_128*nb = Right (Key128 k)
    | otherwise = Left "Wrong Key size"
makeKey AES192 k
    | BS.length k == nk_192*nb = Right (Key192 k)
    | otherwise = Left "Wrong Key size"
makeKey AES256 k
    | BS.length k == nk_256*nb = Right (Key256 k)
    |otherwise = Left "Wrong Key size"

keyaes :: AESKey -> AES
keyaes (Key128 _) = AES128
keyaes (Key192 _) = AES192
keyaes (Key256 _) = AES256

getkey :: AESKey -> Key
getkey (Key128 k) = k
getkey (Key192 k) = k
getkey (Key256 k) = k

type PlainText = BS.ByteString
type Key = BS.ByteString
type CipherText = BS.ByteString

