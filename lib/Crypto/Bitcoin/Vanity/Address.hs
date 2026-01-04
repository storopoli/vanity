{- |
Module: Crypto.Bitcoin.Vanity.Address
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Unified Bitcoin address encoding API.

This module provides a single entry point for encoding public keys
as Bitcoin addresses in any of the supported formats.
-}
module Crypto.Bitcoin.Vanity.Address (
  -- * Unified Encoding
  encodeAddress,

  -- * P2PKH (Base58Check)
  encodeP2PKH,
  p2pkhVersion,

  -- * P2WPKH (Bech32)
  encodeP2WPKH,
  bech32HRP,

  -- * P2TR (Bech32m)
  encodeP2TR,
  bech32mHRP,
) where

import Crypto.Bitcoin.Vanity.Address.Base58 (encodeP2PKH, p2pkhVersion)
import Crypto.Bitcoin.Vanity.Address.Bech32 (bech32HRP, encodeP2WPKH)
import Crypto.Bitcoin.Vanity.Address.Bech32m (bech32mHRP, encodeP2TR)
import Crypto.Bitcoin.Vanity.Types (
  Address,
  AddressType (..),
  Network,
  PublicKey,
 )

{- | Encode a public key as a Bitcoin address.

This is the main entry point for address encoding. It dispatches
to the appropriate encoding function based on the address type.

==== Examples

>>> import Crypto.Bitcoin.Vanity.Key (derivePublicKey)
>>> let Just pk = derivePublicKey (SecretKey 12345)
>>> encodeAddress Mainnet P2PKH pk
Address "1..."
>>> encodeAddress Mainnet P2WPKH pk
Address "bc1q..."
>>> encodeAddress Mainnet P2TR pk
Address "bc1p..."
-}
encodeAddress :: Network -> AddressType -> PublicKey -> Address
encodeAddress network addrType pk = case addrType of
  P2PKH -> encodeP2PKH network pk
  P2WPKH -> encodeP2WPKH network pk
  P2TR -> encodeP2TR network pk
