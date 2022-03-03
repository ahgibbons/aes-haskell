module Main where

--import System.IO
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Data.HexString

import Tests (tkey,tiv)
import Rjindael
import Types
import Block (cbcEncrypt, cbcDecrypt)

main :: IO ()
main = do
    args <- getArgs
    let flag    = args !! 0
        fpath   = args !! 1
        outpath = args !! 2
    intext <- BS.readFile fpath
    let outtext = case flag of 
                   "-e" ->  cbcEncrypt AES128 tiv tkey intext 
                   "-d" ->  fromJust . cbcDecrypt AES128 tkey $ intext
    BS.writeFile outpath outtext
    putStrLn "Finished."

