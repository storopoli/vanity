{- |
Module: Crypto.Bitcoin.Vanity.Worker.WGPU
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

WGPU GPU backend for vanity address generation.
Provides cross-platform GPU support via Vulkan, Metal, or DirectX.
-}
module Crypto.Bitcoin.Vanity.Worker.WGPU (
  searchVanityWithCallback,
) where

import Crypto.Bitcoin.Vanity.Pattern (PatternError (..))
import Crypto.Bitcoin.Vanity.Types (
  VanityConfig (..),
  VanityResult (..),
 )
import Data.Text qualified as T
import Data.Word (Word64)

{- | Search for a vanity address using WGPU.

This backend uses WebGPU/wgpu-native for cross-platform GPU compute,
supporting Vulkan, Metal, and DirectX backends.
-}
searchVanityWithCallback ::
  VanityConfig ->
  -- | Progress callback (receives attempt count)
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchVanityWithCallback _config _callback = do
  -- TODO: Implement WGPU backend
  -- This requires:
  -- 1. wgpu-hs bindings to wgpu-native
  -- 2. WGSL compute shaders for secp256k1 operations
  -- 3. Buffer management for batch processing
  -- 4. Result streaming back to Haskell

  pure $
    Left $
      InvalidRegex $
        T.pack "WGPU backend not yet implemented"

{-
Implementation notes:

The WGPU backend will use compute shaders written in WGSL.
Key challenges:

1. 256-bit arithmetic in WGSL (only has u32)
   - Use 8x32-bit limbs with explicit carry propagation

2. secp256k1 operations
   - Field arithmetic (mod p)
   - Group operations (point add, double)
   - Scalar multiplication with precomputed table

3. Hash functions
   - SHA256 in WGSL
   - RIPEMD160 in WGSL

4. Address encoding
   - Base58Check in WGSL (for P2PKH)
   - Bech32/Bech32m in WGSL (for P2WPKH/P2TR)

5. Pattern matching
   - Simple prefix/suffix matching on GPU
   - Complex regex fallback to CPU

The shader code is in shaders/*.wgsl
-}
