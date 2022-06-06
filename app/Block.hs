{-# LANGUAGE OverloadedStrings #-}
module Block where


import Data.List.Split (chunksOf)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import qualified Data.Matrix as M
import qualified Data.Word as W

import Rjindael
import KeyExpansion (expandKey)
import Utils ( chunksOfBS, blocksize, pkcs7
             , unpkcs7, isValidKey)
import Types


-- Electronic CodeBook Mode. Not recommended for actual use. 
-- This assumes that PKCS7 padding will be used.
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
  | otherwise          = Left "Incorrect key length"


-- Cipher Block Chaining Mode (CBC). Considered secure, but cannot be run in parallel.
cbcEnc :: AES -> Pad -> Key -> IV -> PlainText -> Either String CipherText
cbcEnc aes pad key iv ptext
  | not $ isValidKey aes key    = Left "Incorrect key length"
  | BS.length iv /= blocksize   = Left "Incorrect IV size"
  | (BS.length ptext `mod` blocksize) /= 0  
                && pad == NoPad = Left "Plain text is not a multiple of 16-bytes in size, and no padding."
  | otherwise     = let (nk, nb, nr) = aesparams aes
                        roundKeys = expandKey nk nb nr (BS.unpack key)
                        ptext' = chunksOfBS blocksize $ case pad of NoPad -> ptext
                                                                    PKCS7 -> pkcs7 blocksize ptext
                        cbcstep pt ct = (encryptAES aes roundKeys) (pt `xor` ct) 
                    in Right . mconcat . tail . scanl cbcstep iv $ ptext'

cbcDec :: AES -> Pad -> Key -> IV -> CipherText -> Either String PlainText
cbcDec aes pad key iv ctext
  | not $ isValidKey aes key    = Left "Incorrect key length"
  | BS.length iv /= blocksize   = Left "Incorrect IV size"
  | (BS.length ctext `mod` blocksize) /= 0  = Left "Cipher text is not a multiple of 16-bytes in size"
  | otherwise = let (nk, nb, nr) = aesparams aes
                    roundkeys = reverse $ expandKey nk nb nr (BS.unpack key)
                    ctext' = chunksOfBS blocksize ctext
                    decstep ct0 ct1 = ct0 `xor` (decryptAES aes roundkeys ct1)
                    ptext' = mconcat . zipWith decstep (iv : ctext') $ ctext'
                in case pad of NoPad -> Right ptext'
                               PKCS7 -> unpkcs7 ptext'


ctrEncDec :: AES -> Key -> Nonce -> IV -> PlainText -> Either String CipherText
ctrEncDec aes key nonce iv ptext 
  | not $ isValidKey aes key                        = Left "Incorrect key length"
  | (BS.length iv /= 8) || (BS.length nonce /= 4) = Left "Incorrect Nonce or IV size"
  | otherwise  = let (nk, nb, nr) = aesparams aes
                     roundkeys = expandKey nk nb nr (BS.unpack key)
                     ptext' = chunksOfBS blocksize ptext
                     ctrbs = map (BS.concat . BL.toChunks . runPut . putWord32be) [1..]
                     ctrhead = BS.concat [nonce, iv]
                     ctrblocks = map (BS.append ctrhead) ctrbs
                     ctext' = map (encryptAES aes roundkeys) ctrblocks
                     ctext  = zipWith xor ptext' ctext'
                in Right $ mconcat ctext

{-ctrDec :: AES -> Pad -> Key -> Nonce -> IV -> CipherText -> Either String PlainText
ctrDec aes pad key nonce iv ctext
  | not $ isValidKey aes key    = Left "Incorrect key length"
  | BS.length iv /= blocksize   = Left "Incorrect IV size"
  | (BS.length ctext `mod` blocksize) /= 0  = Left "Cipher text is not a multiple of 16-bytes in size"
  | otherwise   = let (nk, nb, nr) = aesparams aes-}


---  Galois Counter Block Mode
--gcEnc :: AES -> Key -> IV -> AAD -> PlainText 
--      -> (CipherText, AuthTag)



--gcDec :: AES -> Key -> IV -> AAD -> CipherText ->
--          AuthTag -> Maybe PlainText

