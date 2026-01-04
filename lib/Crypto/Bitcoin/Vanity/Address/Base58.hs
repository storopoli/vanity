{-# LANGUAGE BangPatterns #-}

{- |
Module: Crypto.Bitcoin.Vanity.Address.Base58
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

P2PKH (Pay-to-Public-Key-Hash) address encoding using Base58Check.

P2PKH addresses are the original Bitcoin address format, starting with:
- '1' on mainnet
- 'm' or 'n' on testnet
-}
module Crypto.Bitcoin.Vanity.Address.Base58 (
  -- * Address Encoding
  encodeP2PKH,

  -- * Version Bytes
  p2pkhVersion,
) where

import Crypto.Bitcoin.Vanity.Key (serializePublicKey)
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  Network (..),
  PublicKey,
 )
import Crypto.Hash.RIPEMD160 qualified as RIPEMD160
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base58Check qualified as B58Check
import Data.Text.Encoding (decodeUtf8)

{- | Version byte for P2PKH addresses.

- Mainnet: 0x00 (addresses start with '1')
- Testnet: 0x6F (addresses start with 'm' or 'n')
-}
p2pkhVersion :: Network -> ByteString
p2pkhVersion Mainnet = BS.singleton 0x00
p2pkhVersion Testnet = BS.singleton 0x6F

{- | Encode a public key as a P2PKH (Base58Check) address.

The encoding process:

1. Serialize the public key (33 bytes compressed)
2. Compute SHA256 hash (32 bytes)
3. Compute RIPEMD160 hash of SHA256 result (20 bytes) = "Hash160"
4. Prepend version byte
5. Base58Check encode (adds 4-byte checksum)
-}
encodeP2PKH :: Network -> PublicKey -> Address
encodeP2PKH network pk =
  let !pubBytes = serializePublicKey pk
      !sha256Hash = SHA256.hash pubBytes
      !hash160 = RIPEMD160.hash sha256Hash
      !versioned = p2pkhVersion network <> hash160
      !encoded = B58Check.encode versioned
   in Address (decodeUtf8 encoded)
