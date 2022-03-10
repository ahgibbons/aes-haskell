{-# LANGUAGE OverloadedStrings #-}
module Block where


import Data.List.Split (chunksOf)
import Data.Bits (xor)
import qualified Data.ByteString as BS

import Rjindael
import KeyExpansion (expandKey)
import Utils ( chunksOfBS, blocksize, pkcs7
             , unpkcs7)
import Types

ecbEncrypt :: AESKey -> PlainText -> CipherText
ecbEncrypt ckey ptext = BS.concat ctext'
  where
    roundKeys = case ckey of 
                  Key128 key -> expandKey nk_128 nb nr_128 (BS.unpack key)
                  Key192 key -> expandKey nk_192 nb nr_192 (BS.unpack key)
                  Key256 key -> expandKey nk_256 nb nr_256 (BS.unpack key)
    ptext' = chunksOfBS blocksize $ pkcs7 blocksize ptext
    ctext' = map (encryptAES (keyaes ckey) roundKeys) ptext'

ecbDecrypt :: AESKey -> CipherText -> Maybe PlainText
ecbDecrypt ckey ctext = unpkcs7 $ BS.concat ptext'
  where
    roundKeys = case ckey of 
                  Key128 key -> reverse $ expandKey nk_128 nb nr_128 (BS.unpack key)
                  Key192 key -> reverse $ expandKey nk_192 nb nr_192 (BS.unpack key)
                  Key256 key -> reverse $ expandKey nk_256 nb nr_256 (BS.unpack key)
    ctext' = chunksOfBS blocksize ctext
    ptext' = map (decryptAES (keyaes ckey) roundKeys) ctext'

{- ecbEncrypt :: AES -> Key -> PlainText -> CipherText
ecbEncrypt aestype key ptext = BS.concat ctext'
  where
    roundKeys = case aestype of 
                  AES128 -> expandKey nk_128 nb nr_128 (BS.unpack key)
                  AES192 -> expandKey nk_192 nb nr_192 (BS.unpack key)
                  AES256 -> expandKey nk_256 nb nr_256 (BS.unpack key)
    ptext' = chunksOfBS blocksize $ pkcs7 blocksize ptext
    ctext' = map (encryptAES aestype roundKeys) ptext' 
    
ecbDecrypt :: AES -> Key -> CipherText -> Maybe PlainText
ecbDecrypt aestype key ctext = unpkcs7 $ BS.concat ptext'
  where
    roundKeys = case aestype of 
                  AES128 -> reverse $ expandKey nk_128 nb nr_128 (BS.unpack key)
                  AES192 -> reverse $ expandKey nk_192 nb nr_192 (BS.unpack key)
                  AES256 -> reverse $ expandKey nk_256 nb nr_256 (BS.unpack key)
    ctext' = chunksOfBS blocksize ctext
    ptext' = map (decryptAES aestype roundKeys) ctext'
    
    -}

cbcEncrypt :: AES -> IV -> Key -> PlainText -> CipherText
cbcEncrypt aestype iv key ptext = BS.concat ctext'
  where
    roundKeys = case aestype of 
                  AES128 -> expandKey nk_128 nb nr_128 (BS.unpack key)
                  AES192 -> expandKey nk_192 nb nr_192 (BS.unpack key)
                  AES256 -> expandKey nk_256 nb nr_256 (BS.unpack key)
    ptext' = chunksOfBS blocksize . pkcs7 blocksize $ ptext
    ctext' = scanl (\c0 c1 -> encryptAES aestype roundKeys (xor c0 c1)) iv ptext'

cbcDecrypt :: AES -> Key -> CipherText -> Maybe PlainText
cbcDecrypt aestype key ctext = unpkcs7 $ BS.concat ptext'
  where
    roundKeys = case aestype of 
                  AES128 -> reverse $ expandKey nk_128 nb nr_128 (BS.unpack key)
                  AES192 -> reverse $ expandKey nk_192 nb nr_192 (BS.unpack key)
                  AES256 -> reverse $ expandKey nk_256 nb nr_256 (BS.unpack key)
    ctext' = chunksOfBS blocksize ctext
    ptext' = zipWith (\ca cb -> xor (decryptAES aestype roundKeys cb) ca) ctext' (tail ctext')
    
