{-# LANGUAGE OverloadedStrings #-}
module Main where

--import System.IO
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Either (fromRight)

import Tests (tkey,tiv)
import Rjindael
import Types
import Block (cbcEncrypt, cbcDecrypt)

--import Criterion.Main

-- Ideal usage
-- ecbEncrypt key plaintext

main :: IO ()
main = do
    args <- getArgs
    let flag    = args !! 0
        fpath   = args !! 1
        outpath = args !! 2
    intext <- BS.readFile fpath
    let outtext = case flag of 
                   "-e" ->  cbcEncrypt AES128 tiv tkey intext 
                   "-d" ->  fromRight "" . cbcDecrypt AES128 tkey $ intext
    BS.writeFile outpath outtext
    putStrLn "Finished."


{-main = defaultMain [
           bgroup "rotate" [ bench "abcdefg"  $ nf (rotateList 5) "abcdefg"
               , bench "4rf8uhf36gr"  $ nf (rotateList 3) "4rf8uhf36gr"
               , bench "jn6789okj4567654egh8uhgftytr"  $ nf (rotateList 10) "jn6789okj4567654egh8uhgftytr"
               , bench "long"  $ nf (rotateList 8) "a6yg4567tgsghutjhtgfbhyhtgre57ytrsgrthy78u76ytgarsevrydtuk7iuyrrgghts6uthnyjkyt"
               ],
           bgroup "rotate2" [ bench "abcdefg"  $ nf (rotateList2 5) "abcdefg"
               , bench "4rf8uhf36gr"  $ nf (rotateList2 3) "4rf8uhf36gr"
               , bench "jn6789okj4567654egh8uhgftytr"  $ nf (rotateList2 10) "jn6789okj4567654egh8uhgftytr"
               , bench "Long"  $ nf (rotateList2 8) "a6yg4567tgsghutjhtgfbhyhtgre57ytrsgrthy78u76ytgarsevrydtuk7iuyrrgghts6uthnyjkyt"
               ]
            
  ]-}


