{-# LANGUAGE OverloadedStrings #-}
module Main where

--import System.IO
import Options.Applicative
import Data.Semigroup ((<>))
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Either
import Data.Maybe (fromJust)

import Tests
import Rjindael
import Types
import Block

--import Criterion.Main

-- Ideal usage
-- ecbEncrypt key plaintext\

{-main :: IO ()
main = putStrLn "Hello"-}

main :: IO ()
main = runEncrypter =<< execParser opts
  where
    opts = info (mainparser <**> helper)
      ( fullDesc
     <> progDesc "Encrypt a file using AES (ECB or CBC mode)"
     <> header "AES Encrypt" )

{-data Arguments = Arguments {infile_ :: FilePath,
                            encryptFlag_ :: Bool,
                            outfile_ :: FilePath,
                            blockmode_ :: String,
                            keyfile_  :: FilePath,
                            ivfile_   :: FilePath
                            } deriving (Show, Eq)-}



data AESArguments = AESArguments 
                {infile_     :: FilePath,
                outfile_     :: FilePath,
                encryptFlag_ :: EncDec,
                blockmode_   :: String,
                ivfile_      :: FilePath,
                keyfile_     :: FilePath
                } deriving (Show, Eq)

-- ModeParser = ECBMode | CBCMode ivfile
data BlockMode = ECBMode | CBCMode FilePath deriving (Show, Eq)

data EncDec = Encrypt | Decrypt deriving (Show, Eq)

mainparser :: Parser AESArguments
mainparser = AESArguments
      <$> strOption
          ( long "infile"
         <> metavar "INFILE"
         <> help "Path to file to encrypt/decrypt." )
      <*> strOption
          ( long "outfile"
         <> metavar "OUTFILE"
         <> help "Path of output encrypted/decrypted file.")
      <*> flag Encrypt Decrypt
          ( long "decrypt"
         <> short 'd'
         <> help "Set to decrypt (default encrypt)" )
      <*> strOption
          ( long "mode"
         <> metavar "BLOCKMODE"
         <> help "Block cipher mode, ecb or cbc")
      <*> strOption
          ( long "keyfile"
         <> metavar "KEY"
         <> help "Path to key file.")
      <*> strOption
          ( long "ivfile"
         <> metavar "IV"
         <> value ""
         <> help "Path to IV value.")

encrypter :: AES -> Key -> PlainText -> String 
          -> Maybe IV -> Either String CipherText 
encrypter aesmode key ptext bmode iv'
    | bmode == "ecb" = ecbEnc aesmode key ptext
    | bmode == "cbc" = cbcEnc aesmode PKCS7 key (fromJust iv') ptext
    | otherwise      = error "Invalid block mode."

decrypter :: AES -> Key -> Maybe IV -> CipherText 
          -> String -> Either String PlainText
decrypter aesmode key iv' ctext bmode
    | bmode == "ecb" = ecbDec aesmode key ctext
    | bmode == "cbc" = cbcDec aesmode PKCS7 key (fromJust iv') ctext
    | otherwise      = error "Invalid block mode." 

runEncrypter :: AESArguments -> IO ()
runEncrypter args = do
    let encf  = encryptFlag_ args
        mode  = blockmode_ args
        ofile = outfile_ args

    key    <- BS.readFile $ keyfile_ args

    if (mode == "cbc") && (ivfile_ args == "") then error "No IV file."
                                               else return ()
    iv     <- if mode=="cbc" then (fmap Just . BS.readFile $ ivfile_ args)
                          else (return Nothing)
    intext <- BS.readFile $ infile_ args

    let otext = case encf of 
                    Decrypt -> decrypter AES128 key iv intext mode 
                    Encrypt -> encrypter AES128 key intext mode iv

    BS.writeFile ofile (fromRight "" otext)
    


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


