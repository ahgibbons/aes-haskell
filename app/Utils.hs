{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils where

import qualified Data.ByteString as BS
import qualified Data.Bits as B
import Data.Bits (xor)
import Data.Word
import qualified Data.Matrix as Mat
import qualified Data.Vector.Unboxed as V
import Numeric (showHex,showIntAtBase)
import Data.Int (Int64)
import Data.List (unfoldr)
import qualified System.Random as R

import qualified Data.Vector.Storable as VS

import BakeVector
import Data.FileEmbed (embedFile)

import Types
import GaloisFields

blocksize = 16 :: Int


rconTable :: V.Vector Word8
rconTable = V.fromList [ 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40
            , 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a
            , 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a
            , 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39 
            , 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25
            , 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a
            , 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08
            , 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8 
            , 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6
            , 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef
            , 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61
            , 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc 
            , 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01
            , 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b 
            , 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e
            , 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3 
            , 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4
            , 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94 
            , 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8
            , 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20 
            , 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d
            , 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35 
            , 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91
            , 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f 
            , 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d
            , 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04 
            , 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c
            , 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63 
            , 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa
            , 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd 
            , 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66
            , 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d]




rcon_baked :: VS.Vector Word8
rcon_baked = $$(bake rcon_vector)


rcon :: Int -> Word8
rcon n = rcon_vector VS.! n

mxor :: Mat.Matrix Word8 -> Mat.Matrix Word8 -> Mat.Matrix Word8
mxor = Mat.elementwise xor

addRoundKey :: Mat.Matrix Word8 -> Mat.Matrix Word8 -> Mat.Matrix Word8
addRoundKey a b = mxor a b

subBytes :: Mat.Matrix Word8 -> Mat.Matrix Word8
subBytes = fmap subByte 

subByte :: Word8 -> Word8
subByte = BS.index subByteFile . fromIntegral

subByteFile :: BS.ByteString
subByteFile = $(embedFile "subBytes.hb")

invSubByteFile :: BS.ByteString
invSubByteFile = $(embedFile "invSubBytes.hb")


invSubByte :: Word8 -> Word8
invSubByte = BS.index invSubByteFile . fromIntegral


invSubBytes :: Mat.Matrix Word8 -> Mat.Matrix Word8
invSubBytes = fmap invSubByte


rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

           
shiftRows :: Mat.Matrix Word8 -> Mat.Matrix Word8
shiftRows m = Mat.fromLists . zipWith rotateList [0..3] $ Mat.toLists m

invShiftRows :: Mat.Matrix Word8 -> Mat.Matrix Word8
invShiftRows m = Mat.fromLists . zipWith rotateList [4,3..1] $ Mat.toLists m
             
mixColumn :: [Word8] -> [Word8]
mixColumn [s0,s1,s2,s3] = [s0', s1', s2', s3']
  where
    s0' = 0x2 .*. s0  .+. 0x3 .*. s1  .+. s2  .+. s3
    s1' = s0  .+. 0x2 .*. s1  .+. 0x3 .*. s2  .+. s3
    s2' = s0  .+. s1  .+. 0x2 .*. s2  .+. 0x3 .*. s3
    s3' = 0x3 .*. s0  .+. s1  .+. s2  .+. 0x2 .*. s3

mixColumns :: Mat.Matrix Word8 -> Mat.Matrix Word8
mixColumns m = Mat.transpose . Mat.fromLists . map mixColumn
             . Mat.toLists . Mat.transpose $ m

invMixColumn :: [Word8] -> [Word8]
invMixColumn [s0,s1,s2,s3] = [s0',s1',s2',s3']
  where
    s0' = 0x0e .*. s0 .+. 0x0b .*. s1 .+. 0x0d .*. s2 .+. 0x09 .*. s3
    s1' = 0x09 .*. s0 .+. 0x0e .*. s1 .+. 0x0b .*. s2 .+. 0x0d .*. s3
    s2' = 0x0d .*. s0 .+. 0x09 .*. s1 .+. 0x0e .*. s2 .+. 0x0b .*. s3
    s3' = 0x0b .*. s0 .+. 0x0d .*. s1 .+. 0x09 .*. s2 .+. 0x0e .*. s3

invMixColumns :: Mat.Matrix Word8 -> Mat.Matrix Word8
invMixColumns m = Mat.transpose . Mat.fromLists . map invMixColumn
                . Mat.toLists . Mat.transpose $ m

subWord :: [Word8] -> [Word8]
subWord = map subByte

rotWord :: [Word8] -> [Word8]
rotWord [a0,a1,a2,a3] = [a1,a2,a3,a0]

hexMatrix :: Mat.Matrix Word8 -> Mat.Matrix String
hexMatrix = fmap (\a -> showHex a "")

pkcs7 :: Int -> BS.ByteString -> BS.ByteString
pkcs7 blocksize bs = BS.append bs . BS.replicate n . fromIntegral $ n
  where
    n = fromIntegral blocksize - (BS.length bs `mod` fromIntegral blocksize)

unpkcs7 :: BS.ByteString -> Maybe BS.ByteString
unpkcs7 bs = if (BS.all (==last') pad)
             then Just text
             else Nothing
  where
    len        = BS.length bs
    last'      = BS.last bs
    (text,pad) = BS.splitAt (len - (fromIntegral last')) bs

justWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
justWhen f g a = if f a then Just (g a) else Nothing

nothingWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
nothingWhen f = justWhen (not . f)

chunksOfBS :: Int -> BS.ByteString -> [BS.ByteString]
chunksOfBS x = unfoldr (nothingWhen BS.null (BS.splitAt x))

randomIV :: R.RandomGen g => g -> IV
randomIV = BS.pack . take 16 . R.randoms 
