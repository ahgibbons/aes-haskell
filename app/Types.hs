module Types where
import qualified Data.ByteString as BS

nb = 4 :: Int
nk_128 = 4 :: Int
nk_192 = 6 :: Int
nk_256 = 8 :: Int
nr_128 = 10 :: Int
nr_192 = 12 :: Int
nr_256 = 14 :: Int

type Rounds    = Int
type BlockSize = Int
type KeySize   = Int
type IV = BS.ByteString
type PlainText = BS.ByteString
type Key = BS.ByteString
type CipherText = BS.ByteString
type AAD = BS.ByteString
type AuthTag = BS.ByteString

type CounterFunc = Int -> BS.ByteString

data Pad = NoPad | PKCS7 deriving (Show, Eq)

data AES = AES128 | AES192 | AES256 deriving (Show,Eq)
aesparams :: AES -> (KeySize, BlockSize, Rounds)
aesparams AES128 = (nk_128, nb, nr_128)
aesparams AES192 = (nk_192, nb, nr_192)
aesparams AES256 = (nk_256, nb, nr_256)