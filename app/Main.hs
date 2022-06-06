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
    opts = info (commandlineParser <**> helper)
      ( fullDesc
     <> progDesc "Encrypt a file using AES (ECB, CBC or CTR mode)"
     <> header "AES Encrypt" )

-- Data types for commandline arguments
data EncDec = Encrypt | Decrypt deriving (Show, Eq)
data AESArguments = AESArguments 
                {infile_     :: FilePath,
                outfile_     :: FilePath,
                encryptFlag_ :: EncDec,
                blockmode_   :: String,
                keyfile_     :: FilePath,
                ivfile_      :: FilePath,
                keylen_      :: String
                } deriving (Show, Eq)


commandlineParser :: Parser AESArguments
commandlineParser = AESArguments
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
         <> help "Block cipher mode, 'ecb', 'cbc' or 'ctr'")
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



runEncrypter :: AESArguments -> IO ()
runEncrypter args = do
    let encf  = encryptFlag_ args
        mode  = blockmode_ args
        ofile = outfile_ args

    key    <- BS.readFile $ keyfile_ args

    iv     <- case mode of
                "cbc" -> fmap Just . BS.readFile $ infile_ args
                "ctr" -> fmap Just . BS.readFile $ infile_ args
                otherwise -> return Nothing

    {-iv     <- if mode=="cbc" then (fmap Just . BS.readFile $ ivfile_ args)
                          else (return Nothing)-}
    intext <- BS.readFile $ infile_ args
    let keylen = case keylen_ args of
                    "128" -> AES128
                    "192" -> AES192
                    "256" -> AES256
                    _     -> error "Invalid key length." 

    let otext = case encf of 
                    Decrypt -> decrypter keylen key intext mode iv
                    Encrypt -> encrypter keylen key intext mode iv
    case otext of
        Left s -> do putStrLn "Error!"
                     putStrLn s
        Right a -> BS.writeFile ofile a 


--General encryption function
encrypter :: AES -> Key -> PlainText -> String 
          -> Maybe IV -> Either String CipherText 
encrypter aesmode key ptext bmode iv'
    | bmode == "ecb" = ecbEnc aesmode key ptext
    | bmode == "cbc" = cbcEnc aesmode PKCS7 key (fromJust iv') ptext
    | bmode == "ctr" = ctrEncDec aesmode key (BS.take 4 $ fromJust iv') (BS.drop 4 $ fromJust iv') ptext
    | otherwise      = error "Invalid block mode."

--General decryption function
decrypter :: AES -> Key -> CipherText -> String 
          -> Maybe IV -> Either String PlainText
decrypter aesmode key ctext bmode iv'
    | bmode == "ecb" = ecbDec aesmode key ctext
    | bmode == "cbc" = cbcDec aesmode PKCS7 key (fromJust iv') ctext
    | bmode == "ctr" = ctrEncDec aesmode key (BS.take 4 $ fromJust iv') (BS.drop 4 $ fromJust iv') ctext
    | otherwise      = error "Invalid block mode." 