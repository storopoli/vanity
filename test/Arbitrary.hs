{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: Arbitrary
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

QuickCheck Arbitrary instances for vanity address types.
-}
module Arbitrary (
  -- * Re-exports for convenience
  Arbitrary (..),
  Gen,
) where

import Crypto.Bitcoin.Vanity.Types (
  AddressType (..),
  Network (..),
  PublicKey (..),
  SecretKey (..),
  curveOrder,
 )
import Crypto.Curve.Secp256k1 (mul, _CURVE_G)
import Data.Maybe (fromMaybe)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  choose,
  elements,
 )

{- | Arbitrary instance for SecretKey.

Generates valid secret keys in the range (0, curveOrder).
-}
instance Arbitrary SecretKey where
  arbitrary :: Gen SecretKey
  arbitrary = SecretKey <$> choose (1, curveOrder - 1)

{- | Arbitrary instance for PublicKey.

Derives a public key from an arbitrary secret key.
-}
instance Arbitrary PublicKey where
  arbitrary :: Gen PublicKey
  arbitrary = do
    scalar <- choose (1, curveOrder - 1)
    let point = fromMaybe (error "Failed to derive public key") $ mul _CURVE_G (fromInteger scalar)
    pure (PublicKey point)

-- | Arbitrary instance for Network.
instance Arbitrary Network where
  arbitrary :: Gen Network
  arbitrary = elements [Mainnet, Testnet]

-- | Arbitrary instance for AddressType.
instance Arbitrary AddressType where
  arbitrary :: Gen AddressType
  arbitrary = elements [P2PKH, P2WPKH, P2TR]
