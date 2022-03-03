{-# LANGUAGE OverloadedStrings #-}
module Rjindael where

import qualified Data.ByteString as BS
import qualified Data.Bits as B
import Data.Bits ((.&.),(.|.),xor)


import Data.Char (intToDigit)
import Data.Word
import Data.Bits.ByteString
import Data.List.Split (chunksOf)
import qualified Data.Matrix as Mat


import Utils (mxor, subBytes, shiftRows, invSubBytes, invShiftRows,
             mixColumns, invMixColumns)
import KeyExpansion (expandKey)
import Types
import GaloisFields


nb = 4 :: Int
nk_128 = 4 :: Int
nk_192 = 6 :: Int
nk_256 = 8 :: Int
nr_128 = 10 :: Int
nr_192 = 12 :: Int
nr_256 = 14 :: Int


multParts :: Int -> Int -> Int -> [Int]
multParts n a b 
  | n > a     = []
  | otherwise = (n .&. a) * b : multParts (B.shiftL n 1) a b

rotateByteStringL :: Int -> BS.ByteString -> BS.ByteString
rotateByteStringL i bs = BS.append (BS.drop i bs) (BS.take i bs)


inputToStateMatrix :: BS.ByteString -> Mat.Matrix Word8
inputToStateMatrix bs = Mat.transpose . Mat.fromList 4 4 $ BS.unpack bs

stateMatrixToOutput :: Mat.Matrix Word8 -> BS.ByteString
stateMatrixToOutput mat = BS.pack . Mat.toList 
                        . Mat.transpose $ mat


encryptAES :: AES -> [Mat.Matrix Word8] -> PlainText -> CipherText
encryptAES aestype roundKeys text = stateMatrixToOutput . mxor rf 
                . subBytes . shiftRows 
                . foldl cipherRound initState $ rs
  where
    (nk,nr) = case aestype of
                   AES128 -> (nk_128, nr_128)
                   AES192 -> (nk_192, nr_192)
                   AES256 -> (nk_256, nr_256)
    initState = mxor (inputToStateMatrix text) r0
    r0 = head roundKeys
    rs = take (nr-1) . tail $ roundKeys
    rf = roundKeys !! nr
              
decryptAES :: AES -> [Mat.Matrix Word8] -> CipherText -> PlainText
decryptAES aestype roundKeys text = stateMatrixToOutput . mxor rf . invSubBytes
                   . invShiftRows . foldl invCipherRound initState $ rs
  where
    (nk,nr) = case aestype of
                AES128 -> (nk_128, nr_128)
                AES192 -> (nk_192, nr_192)
                AES256 -> (nk_256, nr_256)
    r0 = head roundKeys
    rs = take (nr-1) . tail $ roundKeys
    rf = roundKeys !! nr
    initState = mxor (inputToStateMatrix text) r0


cipherRound instate roundkey = mxor roundkey . mixColumns
                           . shiftRows . subBytes $ instate

invCipherRound instate roundkey = invMixColumns . mxor roundkey
                                . invSubBytes . invShiftRows $ instate


