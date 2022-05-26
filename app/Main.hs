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

main :: IO ()
main = runEncrypter =<< execParser opts
  where
    opts = info (mainparser <**> helper)
      ( fullDesc
     <> progDesc "Encrypt a file using AES (ECB or CBC mode)"
     <> header "AES Encrypt" )


data AESArguments = AESArguments 
                {infile_     :: FilePath,
                outfile_     :: FilePath,
                encryptFlag_ :: EncDec,
                blockmode_   :: String,
                keyfile_     :: FilePath,
                ivfile_      :: FilePath,
                keylen_      :: String
                } deriving (Show, Eq)


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
      <*> strOption
          ( long "keylen"
         <> metavar "KEYLEN"
         <> value "128"
         <> help "AES key length (128 (default) or 192 or 256)")

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

    iv     <- if mode=="cbc" then (fmap Just . BS.readFile $ ivfile_ args)
                          else (return Nothing)
    intext <- BS.readFile $ infile_ args
    let keylen = case keylen_ args of
                    "128" -> AES128
                    "192" -> AES192
                    "256" -> AES256
                    _     -> error "Invalid key length." 


    let otext = case encf of 
                    Decrypt -> decrypter keylen key iv intext mode 
                    Encrypt -> encrypter keylen key intext mode iv
    case otext of
        Left s -> do putStrLn "Error!"
                     putStrLn s
        Right a -> BS.writeFile ofile a 