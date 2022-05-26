# AES in Haskell

This is an implementation of AES in Haskell. This was a personal learning exercise and is not intended for real world use.

## Installation

The project uses cabal. To install use:

`cabal build` or `cabal install`

## Running program

To run (for example after using `cabal build`)

`cabal run aes -- ARGS`


Command line arguments:

```AES Encrypt

Usage: aes --infile INFILE --outfile OUTFILE [-d|--decrypt] --mode BLOCKMODE
           --keyfile KEY [--ivfile IV]

  Encrypt a file using AES (ECB or CBC mode)

Available options:
  --infile INFILE          Path to file to encrypt/decrypt.
  --outfile OUTFILE        Path of output encrypted/decrypted file.
  -d,--decrypt             Set to decrypt (default encrypt)
  --mode BLOCKMODE         Block cipher mode, ecb or cbc
  --keyfile KEY            Path to key file.
  --ivfile IV              Path to IV value.
  --keylen KEYLEN          AES key length (128 (default) or 192 or 256)
  -h,--help                Show this help text
```
 
As an example, to encrypt a file using CBC mode where the key is in `keyfile.key` and the iv is in `iv.iv`:
  
`cabal run aes -- --infile plaintext.bin --outfile ciphertext.aes --mode cbc --keyfile keyfile.key --ivfile iv.iv`

To decrypt this file:

`cabal run aes -- -d --infile ciphertext.aes --outfile plaintext_out.bin --mode cbc --keyfile keyfile.key --ivfile iv.iv`

## Wishlist

* Add counter block mode
* Add Galois counter block mode
* Include a key derivation function to allow user passwords instead of a key file
* Create randomly generated IVs for cbc mode
