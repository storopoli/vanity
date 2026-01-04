{-# LANGUAGE BangPatterns #-}

{- |
Module: Crypto.Bitcoin.Vanity.Address.Bech32
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

P2WPKH (Pay-to-Witness-Public-Key-Hash) address encoding using Bech32.

P2WPKH addresses are SegWit v0 addresses per BIP141/BIP173, starting with:
- "bc1q" on mainnet
- "tb1q" on testnet

The 'q' indicates witness version 0.
-}
module Crypto.Bitcoin.Vanity.Address.Bech32 (
  -- * Address Encoding
  encodeP2WPKH,

  -- * Human-Readable Parts
  bech32HRP,

  -- * Helpers (for Bech32m)
  prependWitnessVersion,
) where

import Crypto.Bitcoin.Vanity.Key (serializePublicKey)
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  Network (..),
  PublicKey,
 )
import Crypto.Hash.RIPEMD160 qualified as RIPEMD160
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Bech32 qualified as Bech32
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)

{- | Human-readable part (HRP) for Bech32 addresses.

- Mainnet: "bc" (addresses start with "bc1q")
- Testnet: "tb" (addresses start with "tb1q")
-}
bech32HRP :: Network -> ByteString
bech32HRP Mainnet = BS.pack [0x62, 0x63] -- "bc"
bech32HRP Testnet = BS.pack [0x74, 0x62] -- "tb"

{- | Prepend a witness version (5-bit value) to a witness program.

The ppad-bech32 library does 8-to-5 bit conversion internally.
For segwit, the witness version must be a raw 5-bit value, not
converted from 8-bit. This function re-packs the data so that
after 8-to-5 conversion, the first 5-bit value is the witness version.

For witness version V and witness program P (n bytes):
- Total bits needed: 5 + 8*n
- Total bytes: ceil((5 + 8*n) / 8)

Bit layout:
- First byte: V[4:0] || P[0][7:5] (5 bits version + 3 bits from first program byte)
- Byte i (i > 0): P[i-1][4:0] || P[i][7:5]
- Last byte: P[n-1][4:0] || 000 (with 3-bit padding)
-}
prependWitnessVersion :: Word8 -> ByteString -> ByteString
prependWitnessVersion version program
  | BS.null program = BS.singleton (version `shiftL` 3)
  | otherwise = BS.pack $ go 0
 where
  len = BS.length program
  go :: Int -> [Word8]
  go i
    | i == 0 =
        -- First byte: version (5 bits) + first 3 bits of program[0]
        let firstByte = (version `shiftL` 3) .|. (BS.index program 0 `shiftR` 5)
         in firstByte : go 1
    | i <= len =
        -- Byte i: last 5 bits of program[i-1] + first 3 bits of program[i]
        let prev = BS.index program (i - 1)
            curr = if i < len then BS.index program i else 0
            byte = (prev `shiftL` 3) .|. (curr `shiftR` 5)
         in byte : go (i + 1)
    | otherwise = []

{- | Encode a public key as a P2WPKH (Bech32) address.

The encoding process:

1. Serialize the public key (33 bytes compressed)
2. Compute SHA256 hash (32 bytes)
3. Compute RIPEMD160 hash of SHA256 result (20 bytes) = "Hash160"
4. Prepend witness version 0 as a 5-bit value
5. Bech32 encode with network HRP

The witness program for P2WPKH is: OP_0 <20-byte-hash>
-}
encodeP2WPKH :: Network -> PublicKey -> Address
encodeP2WPKH network pk =
  let !pubBytes = serializePublicKey pk
      !sha256Hash = SHA256.hash pubBytes
      !hash160 = RIPEMD160.hash sha256Hash
      -- Prepend witness version 0 correctly (as 5-bit value)
      !witnessData = prependWitnessVersion 0 hash160
      !hrp = bech32HRP network
      !encoded = case Bech32.encode hrp witnessData of
        Just bs -> bs
        Nothing -> error "Bech32 encoding failed"
   in Address (decodeUtf8 encoded)
