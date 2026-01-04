{-# LANGUAGE BangPatterns #-}

{- |
Module: Crypto.Bitcoin.Vanity.Address.Bech32m
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

P2TR (Pay-to-Taproot) address encoding using Bech32m.

P2TR addresses are SegWit v1 addresses per BIP341/BIP350, starting with:
- "bc1p" on mainnet
- "tb1p" on testnet

The 'p' indicates witness version 1.

Note: This implementation creates key-path-only Taproot outputs.
For full Taproot with script trees, additional logic would be needed.
-}
module Crypto.Bitcoin.Vanity.Address.Bech32m (
  -- * Address Encoding
  encodeP2TR,

  -- * Human-Readable Parts
  bech32mHRP,
) where

import Crypto.Bitcoin.Vanity.Address.Bech32 (prependWitnessVersion)
import Crypto.Bitcoin.Vanity.Key (serializePublicKeyXOnly)
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  Network (..),
  PublicKey,
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Bech32m qualified as Bech32m
import Data.Text.Encoding (decodeUtf8)

{- | Human-readable part (HRP) for Bech32m addresses.

- Mainnet: "bc" (addresses start with "bc1p")
- Testnet: "tb" (addresses start with "tb1p")

Note: Same HRP as Bech32, but with Bech32m checksum.
-}
bech32mHRP :: Network -> ByteString
bech32mHRP Mainnet = BS.pack [0x62, 0x63] -- "bc"
bech32mHRP Testnet = BS.pack [0x74, 0x62] -- "tb"

{- | Encode a public key as a P2TR (Bech32m/Taproot) address.

The encoding process:

1. Serialize the public key to x-only format (32 bytes)
2. Prepend witness version 1 as a 5-bit value
3. Bech32m encode with network HRP

The witness program for P2TR is: OP_1 <32-byte-x-only-pubkey>

This creates a key-path-only Taproot output where the public key
itself serves as the tweaked output key.
-}
encodeP2TR :: Network -> PublicKey -> Address
encodeP2TR network pk =
  let !xOnlyPubKey = serializePublicKeyXOnly pk
      -- Prepend witness version 1 correctly (as 5-bit value)
      !witnessData = prependWitnessVersion 1 xOnlyPubKey
      !hrp = bech32mHRP network
      !encoded = case Bech32m.encode hrp witnessData of
        Just bs -> bs
        Nothing -> error "Bech32m encoding failed"
   in Address (decodeUtf8 encoded)
