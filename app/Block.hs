{-# LANGUAGE OverloadedStrings #-}
module Block where


import Data.List.Split (chunksOf)
import Data.Bits (xor)
import qualified Data.ByteString as BS

import Rjindael
import KeyExpansion (expandKey)
import Utils ( chunksOfBS, blocksize, pkcs7
             , unpkcs7, isValidKey)
import Types


-- Electronic CodeBook Mode. Not recommended for actual use. 
ecbEnc :: AES -> Key -> PlainText -> Either String CipherText
ecbEnc aes key ptext
  | isValidKey aes key = let (nk, nb, nr) = aesparams aes
                             roundKeys = expandKey nk nb nr (BS.unpack key)
                             ptext' = chunksOfBS blocksize $ pkcs7 blocksize ptext
                             ctext' = map (encryptAES aes roundKeys) ptext' 
                         in Right $ mconcat ctext' 
  | otherwise          = Left "Incorrect key length"


ecbDec :: AES -> Key -> CipherText -> Either String PlainText
ecbDec aes key ctext
  | isValidKey aes key = let (nk, nb, nr) = aesparams aes
                             roundKeys = reverse $ expandKey nk nb nr (BS.unpack key)
                             ctext' = chunksOfBS blocksize ctext
                             ptext' = map (decryptAES aes roundKeys) ctext'
                         in unpkcs7 $ mconcat ptext'
  | otherwise       = Left "Incorrect key length"


-- Cipher Block Chaining Mode (CBC). Considered secure, but cannot be run in parallel.
cbcEncrypt :: AES -> IV -> Key -> PlainText -> CipherText
cbcEncrypt aestype iv key ptext = mconcat ctext'
  where
    roundKeys = case aestype of 
                  AES128 -> expandKey nk_128 nb nr_128 (BS.unpack key)
                  AES192 -> expandKey nk_192 nb nr_192 (BS.unpack key)
                  AES256 -> expandKey nk_256 nb nr_256 (BS.unpack key)
    ptext' = chunksOfBS blocksize . pkcs7 blocksize $ ptext
    ctext' = scanl (\c0 c1 -> encryptAES aestype roundKeys (xor c0 c1)) iv ptext'

cbcDecrypt :: AES -> Key -> CipherText -> Either String PlainText
cbcDecrypt aestype key ctext = unpkcs7 $ BS.concat ptext'
  where
    roundKeys = case aestype of 
                  AES128 -> reverse $ expandKey nk_128 nb nr_128 (BS.unpack key)
                  AES192 -> reverse $ expandKey nk_192 nb nr_192 (BS.unpack key)
                  AES256 -> reverse $ expandKey nk_256 nb nr_256 (BS.unpack key)
    ctext' = chunksOfBS blocksize ctext
    ptext' = zipWith (\ca cb -> xor (decryptAES aestype roundKeys cb) ca) ctext' (tail ctext')