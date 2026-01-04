{-# LANGUAGE BangPatterns #-}

{- |
Module: Crypto.Bitcoin.Vanity.Key
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Key generation and derivation for Bitcoin vanity addresses.
Uses ppad-secp256k1 for elliptic curve operations.
-}
module Crypto.Bitcoin.Vanity.Key (
  -- * Key Generation
  generateSecretKey,

  -- * Key Derivation
  derivePublicKey,

  -- * Serialization
  serializePublicKey,
  serializePublicKeyXOnly,

  -- * WIF Encoding
  toWIF,

  -- * Utilities
  isValidSecretKey,
  bytesToInteger,
  integerToBytes32,
) where

import Crypto.Bitcoin.Vanity.Types (
  Network (..),
  PublicKey (..),
  SecretKey (..),
  curveOrder,
 )
import Crypto.Curve.Secp256k1 (derive_pub, serialize_point)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base58Check qualified as B58Check
import System.Entropy (getEntropy)

{- | Generate a random secret key using system entropy.

Uses the system's cryptographically secure random number generator.
Automatically retries if an invalid key is generated (extremely rare).
-}
generateSecretKey :: IO SecretKey
generateSecretKey = do
  bytes <- getEntropy 32
  let !sk = SecretKey (bytesToInteger bytes)
  if isValidSecretKey sk
    then pure sk
    else generateSecretKey -- Retry if invalid (probability ~2^-128)

{- | Derive a public key from a secret key.

Returns 'Nothing' if the secret key is invalid (zero or >= curve order).
-}
derivePublicKey :: SecretKey -> Maybe PublicKey
derivePublicKey (SecretKey sk) = PublicKey <$> derive_pub (fromInteger sk)

{- | Serialize a public key to 33-byte compressed format.

The first byte is 0x02 (even y) or 0x03 (odd y), followed by
the 32-byte x-coordinate.
-}
serializePublicKey :: PublicKey -> ByteString
serializePublicKey (PublicKey proj) = serialize_point proj

{- | Serialize a public key to 32-byte x-only format (for Taproot/P2TR).

Returns only the x-coordinate without the parity byte.
-}
serializePublicKeyXOnly :: PublicKey -> ByteString
serializePublicKeyXOnly pk = BS.drop 1 (serializePublicKey pk)

{- | Check if a secret key is valid.

A valid secret key is in the range @(0, curveOrder)@.
-}
isValidSecretKey :: SecretKey -> Bool
isValidSecretKey (SecretKey sk) = sk > 0 && sk < curveOrder

{- | Encode a secret key in Wallet Import Format (WIF).

WIF encoding:
1. Version byte: 0x80 (mainnet) or 0xEF (testnet)
2. 32-byte secret key
3. Compression flag: 0x01 (always compressed)
4. Base58Check encode (adds 4-byte checksum)
-}
toWIF :: Network -> SecretKey -> ByteString
toWIF network (SecretKey sk) =
  let versionByte = case network of
        Mainnet -> BS.singleton 0x80
        Testnet -> BS.singleton 0xEF
      keyBytes = integerToBytes32 sk
      compressionFlag = BS.singleton 0x01
      payload = versionByte <> keyBytes <> compressionFlag
   in B58Check.encode payload

-- | Convert a ByteString to an Integer (big-endian).
bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

{- | Convert an Integer to a 32-byte big-endian ByteString.

Pads with leading zeros if necessary.
-}
integerToBytes32 :: Integer -> ByteString
integerToBytes32 i =
  BS.pack $ reverse [fromInteger (i `shiftR` (8 * j)) .&. 0xff | j <- [0 .. 31]]

-- | Compute double SHA256 hash (used for checksum in WIF).
_doubleSHA256 :: ByteString -> ByteString
_doubleSHA256 = SHA256.hash . SHA256.hash
