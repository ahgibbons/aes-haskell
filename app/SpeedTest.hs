module SpeedTest where

import Crypto.Cipher.Types hiding (cbcEncrypt,cbcDecrypt)
import qualified Crypto.Cipher.Types as C
import Crypto.Cipher.AES
import Crypto.Error
import qualified Data.ByteString as BS
import Utils (pkcs7)

import qualified Block as B
import qualified Types as T

controlCipher :: BS.ByteString -> AES128
controlCipher key = case cipherInit key of
                      CryptoPassed cipher -> cipher
                      CryptoFailed _      -> error "Failed cipher"


controlEncrypt :: BS.ByteString -> BS.ByteString 
               -> BS.ByteString -> BS.ByteString
controlEncrypt iv key ptext = C.cbcEncrypt (controlCipher key) iv' (pkcs7 16 ptext)
  where
    Just iv' = makeIV iv 


myEncrypt :: BS.ByteString -> BS.ByteString 
          -> BS.ByteString -> BS.ByteString
myEncrypt iv key ptext = B.cbcEncrypt T.AES128 iv key ptext
