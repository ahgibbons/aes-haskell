{-# LANGUAGE OverloadedStrings #-}
module Tests where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16 (encode, decode)

import KeyExpansion (expandKey)
import Rjindael (encryptAES)
import Block
import Types


tinput :: BS.ByteString
tinput = BS.pack [0x32,0x43,0xf6,0xa8
         ,0x88,0x5a,0x30,0x8d
         ,0x31,0x31,0x98,0xa2
         ,0xe0,0x37,0x07,0x34]

tkey = BS.pack [0x2b,0x7e,0x15,0x16
       ,0x28,0xae,0xd2,0xa6
       ,0xab,0xf7,0x15,0x88
       ,0x09,0xcf,0x4f,0x3c]

tkey_128 = BS.pack [0x2b,0x7e,0x15,0x16
       ,0x28,0xae,0xd2,0xa6
       ,0xab,0xf7,0x15,0x88
       ,0x09,0xcf,0x4f,0x3c]

tkey_192 = BS.pack [0x8e,0x73,0xb0,0xf7
           ,0xda,0x0e,0x64,0x52
           ,0xc8,0x10,0xf3,0x2b
           ,0x80,0x90,0x79,0xe5
           ,0x62,0xf8,0xea,0xd2
           ,0x52,0x2c,0x6b,0x7b]

tkey_256 = BS.pack [0x60,0x3d,0xeb,0x10
           ,0x15,0xca,0x71,0xbe
           ,0x2b,0x73,0xae,0xf0
           ,0x85,0x7d,0x77,0x81
           ,0x1f,0x35,0x2c,0x07
           ,0x3b,0x61,0x08,0xd7
           ,0x2d,0x98,0x10,0xa3
           ,0x09,0x14,0xdf,0xf4]

-- Examples are from NIST, FIPS 197
-- 128-bit key
c1_plaintext  = BS16.decode "00112233445566778899aabbccddeeff"
c1_key        = BS16.decode "000102030405060708090a0b0c0d0e0f"
c1_ciphertext = BS16.decode "69c4e0d86a7b0430d8cdb78070b4c55a"

c1_aeskey = makeKey AES128 =<< c1_key

ct_1 = fmap BS16.encode $ encryptAES AES128 <$> expandKey nk_128 nb nr_128 
                       <$> BS.unpack <$> getkey <$> c1_aeskey <*> c1_plaintext

-- 192-bit key
c2_plaintext  = BS16.decode "00112233445566778899aabbccddeeff"
c2_key        = BS16.decode "000102030405060708090a0b0c0d0e0f1011121314151617"
c2_ciphertext = BS16.decode "dda97ca4864cdfe06eaf70a0ec0d7191"

c2_aeskey = c2_key >>= makeKey AES192

ct_2 = fmap BS16.encode $ encryptAES AES192 <$> expandKey nk_192 nb nr_192 
                       <$> BS.unpack <$> getkey <$> c2_aeskey <*> c2_plaintext

-- 256-bit key
c3_plaintext  = BS16.decode "00112233445566778899aabbccddeeff"
c3_key        = BS16.decode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
c3_ciphertext = BS16.decode "8ea2b7ca516745bfeafc49904b496089"

c3_aeskey = c3_key >>= makeKey AES256

ct_3 = fmap BS16.encode $ encryptAES AES256 <$> expandKey nk_256 nb nr_256 
                       <$> BS.unpack <$> getkey <$> c3_aeskey <*> c3_plaintext

tiv :: BS.ByteString
tiv = "0123456789abcdef"

longText = "Hello World. This is an AES example." :: BS.ByteString
