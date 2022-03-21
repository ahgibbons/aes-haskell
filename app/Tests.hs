{-# LANGUAGE OverloadedStrings #-}
module Tests where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16 (encode, decode)

import Types

-- Examples are from NIST, FIPS 197
-- Below are tests confirming that the AES cipher works as expected.
-- However, this is not how the AES should be used in practice. It should be
-- used with a block cipher mode.
-- 128-bit key
Right c1_plaintext  = BS16.decode "00112233445566778899aabbccddeeff"
Right c1_key        = BS16.decode "000102030405060708090a0b0c0d0e0f"
c1_ciphertext = "69c4e0d86a7b0430d8cdb78070b4c55a" :: BS.ByteString


-- 192-bit key
Right c2_plaintext  = BS16.decode "00112233445566778899aabbccddeeff"
Right c2_key        = BS16.decode "000102030405060708090a0b0c0d0e0f1011121314151617"
c2_ciphertext = "dda97ca4864cdfe06eaf70a0ec0d7191" :: BS.ByteString


-- 256-bit key
Right c3_plaintext  = BS16.decode "00112233445566778899aabbccddeeff"
Right c3_key        = BS16.decode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
c3_ciphertext = "8ea2b7ca516745bfeafc49904b496089" :: BS.ByteString

-----
--- CBC Test Vectors (AES128, no padding), From RFC 3602
-----

Right c4_key = BS16.decode "06a9214036b8a15b512e03d534120006"
Right c4_iv  = BS16.decode "3dafba429d9eb430b422da802c9fac41"
c4_plaintext = "Single block msg" :: BS.ByteString
c4_ciphertext =  BS16.decode "e353779c1079aeb82708942dbe77181a"

Right c5_key = BS16.decode "c286696d887c9aa0611bbb3e2025a45a"
Right c5_iv  = BS16.decode "562e17996d093d28ddb3ba695a2e6f58"
Right c5_plaintext = BS16.decode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
c5_ciphertext = "d296cd94c2cccf8a3a863028b5e1dc0a7586602d253cfff91b8266bea6d61ab1" :: BS.ByteString

Right c6_key = BS16.decode "56e47a38c5598974bc46903dba290349"
Right c6_iv  = BS16.decode"8ce82eefbea0da3c44699ed7db51b7d9"
Right c6_plaintext = BS16.decode "a0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
ciphertext =  "c30e32ffedc0774e6aff6af0869f71aa0f3af07a9a31a9c684db207eb0ef8e4e35907aa632c3ffdf868bb7b29d3d46ad83ce9f9a102ee99d49a53e87f4c3da55" :: BS.ByteString


-----
-- CTR 