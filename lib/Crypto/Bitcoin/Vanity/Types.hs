{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Crypto.Bitcoin.Vanity.Types
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Core types for the Bitcoin vanity address generator.
-}
module Crypto.Bitcoin.Vanity.Types (
  -- * Network
  Network (..),

  -- * Address Types
  AddressType (..),

  -- * Keys
  SecretKey (..),
  PublicKey (..),

  -- * Address
  Address (..),

  -- * Pattern
  Pattern (..),

  -- * Search Result
  VanityResult (..),

  -- * Configuration
  VanityConfig (..),
  defaultConfig,

  -- * Backend Selection
  Backend (..),

  -- * Constants
  curveOrder,
) where

import Control.DeepSeq (NFData (..))
import Crypto.Curve.Secp256k1 (Projective)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Bitcoin network.
data Network
  = -- | Mainnet (production network)
    Mainnet
  | -- | Testnet (test network)
    Testnet
  deriving stock (Show, Eq, Ord, Generic)

instance NFData Network

-- | Bitcoin address type.
data AddressType
  = -- | Pay-to-Public-Key-Hash (Base58Check, legacy addresses starting with '1')
    P2PKH
  | -- | Pay-to-Witness-Public-Key-Hash (Bech32, SegWit v0, addresses starting with 'bc1q')
    P2WPKH
  | -- | Pay-to-Taproot (Bech32m, SegWit v1, addresses starting with 'bc1p')
    P2TR
  deriving stock (Show, Eq, Ord, Generic)

instance NFData AddressType

{- | Secret key (256-bit scalar).

Internally stored as 'Integer' for compatibility with ppad-secp256k1's 'derive_pub'.
Valid range: @0 < sk < curveOrder@.
-}
newtype SecretKey = SecretKey {unSecretKey :: Integer}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num)

instance NFData SecretKey

instance Show SecretKey where
  show _ = "SecretKey <redacted>"

-- | Public key (secp256k1 curve point in projective coordinates).
newtype PublicKey = PublicKey {unPublicKey :: Projective}
  deriving stock (Eq, Generic)

instance NFData PublicKey where
  rnf (PublicKey p) = p `seq` ()

instance Show PublicKey where
  show _ = "PublicKey <...>"

-- | Bitcoin address as text.
newtype Address = Address {unAddress :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NFData)

-- | Regex pattern for matching addresses.
newtype Pattern = Pattern {unPattern :: Text}
  deriving stock (Show, Eq, Ord, Generic)

-- | Result of a successful vanity address search.
data VanityResult = VanityResult
  { vrSecretKey :: SecretKey
  -- ^ The secret key that generates the vanity address
  , vrPublicKey :: PublicKey
  -- ^ The corresponding public key
  , vrAddress :: Address
  -- ^ The matching vanity address
  , vrAttempts :: Word64
  -- ^ Number of attempts before finding the match
  }
  deriving stock (Show, Eq, Generic)

instance NFData VanityResult where
  rnf (VanityResult sk pk addr att) =
    rnf sk `seq` rnf pk `seq` rnf addr `seq` rnf att

-- | Computation backend.
data Backend
  = -- | Multi-threaded CPU
    CPU
  | -- | NVIDIA CUDA GPU
    CUDA
  | -- | WGPU (Metal/Vulkan/DirectX)
    WGPU
  deriving stock (Show, Eq, Ord, Generic)

instance NFData Backend

-- | Configuration for vanity address generation.
data VanityConfig = VanityConfig
  { vcNetwork :: Network
  -- ^ Bitcoin network (mainnet or testnet)
  , vcAddressType :: AddressType
  -- ^ Address type to generate
  , vcPattern :: Pattern
  -- ^ Regex pattern to match
  , vcBackend :: Backend
  -- ^ Computation backend
  , vcThreads :: Maybe Int
  -- ^ Number of threads (Nothing = use all available cores)
  , vcCaseSensitive :: Bool
  -- ^ Whether pattern matching is case-sensitive
  }
  deriving stock (Show, Eq, Generic)

-- | Default configuration with mainnet P2PKH and CPU backend.
defaultConfig :: Pattern -> VanityConfig
defaultConfig pat =
  VanityConfig
    { vcNetwork = Mainnet
    , vcAddressType = P2PKH
    , vcPattern = pat
    , vcBackend = CPU
    , vcThreads = Nothing
    , vcCaseSensitive = True
    }

{- | The secp256k1 curve order (a mathematical constant).

All valid secret keys must be in the range @(0, curveOrder)@.
-}
curveOrder :: Integer
curveOrder = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
