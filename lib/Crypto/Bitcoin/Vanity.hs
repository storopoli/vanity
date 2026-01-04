{- |
Module: Crypto.Bitcoin.Vanity
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Bitcoin vanity address generator.

This is the main public API for the vanity address generator.
It re-exports all the types and functions needed to generate
vanity Bitcoin addresses.

== Quick Start

@
import Crypto.Bitcoin.Vanity

main :: IO ()
main = do
  let config = defaultConfig (Pattern "^1ABC")
  result <- search config
  case result of
    Left err -> print err
    Right VanityResult{..} -> do
      putStrLn $ "Found: " ++ show vrAddress
      putStrLn $ "Attempts: " ++ show vrAttempts
@

== Address Types

The library supports three Bitcoin address types:

* 'P2PKH': Legacy addresses (Base58Check, starting with '1')
* 'P2WPKH': SegWit v0 addresses (Bech32, starting with "bc1q")
* 'P2TR': Taproot addresses (Bech32m, starting with "bc1p")

== Backends

Currently only the CPU backend is implemented. GPU backends
(CUDA and WGPU) are planned for future releases.
-}
module Crypto.Bitcoin.Vanity (
  -- * Types
  module Crypto.Bitcoin.Vanity.Types,

  -- * Key Generation
  module Crypto.Bitcoin.Vanity.Key,

  -- * Address Encoding
  module Crypto.Bitcoin.Vanity.Address,

  -- * Pattern Matching
  module Crypto.Bitcoin.Vanity.Pattern,

  -- * Vanity Search
  module Crypto.Bitcoin.Vanity.Worker,
) where

import Crypto.Bitcoin.Vanity.Address
import Crypto.Bitcoin.Vanity.Key
import Crypto.Bitcoin.Vanity.Pattern
import Crypto.Bitcoin.Vanity.Types
import Crypto.Bitcoin.Vanity.Worker
