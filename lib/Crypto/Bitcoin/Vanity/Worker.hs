{-# LANGUAGE CPP #-}

{- |
Module: Crypto.Bitcoin.Vanity.Worker
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Unified worker interface for vanity address generation.

This module provides a single entry point for vanity address generation
that dispatches to the appropriate backend (CPU, CUDA, or WGPU).
-}
module Crypto.Bitcoin.Vanity.Worker (
  -- * Search Interface
  search,
  searchWithProgress,

  -- * Backend Information
  availableBackends,
) where

import Crypto.Bitcoin.Vanity.Pattern (PatternError)
import Crypto.Bitcoin.Vanity.Types (Backend (..), VanityConfig (..), VanityResult)
import Crypto.Bitcoin.Vanity.Worker.CPU qualified as CPU
import Data.Word (Word64)

#ifdef CUDA_ENABLED
import qualified Crypto.Bitcoin.Vanity.Worker.CUDA as CUDA
#endif

#ifdef WGPU_ENABLED
import qualified Crypto.Bitcoin.Vanity.Worker.WGPU as WGPU
#endif

{- | List of available backends based on compile-time flags.

The CPU backend is always available. CUDA and WGPU are optional
and enabled via cabal flags.
-}
availableBackends :: [Backend]
availableBackends =
  [CPU]
#ifdef CUDA_ENABLED
    ++ [CUDA]
#endif
#ifdef WGPU_ENABLED
    ++ [WGPU]
#endif

{- | Search for a vanity address using the configured backend.

This is the main entry point for vanity address generation.
It dispatches to the appropriate backend implementation.
-}
search :: VanityConfig -> IO (Either PatternError VanityResult)
search config = searchWithProgress config (const $ pure ())

{- | Search for a vanity address with a progress callback.

The callback receives the current attempt count periodically.
-}
searchWithProgress ::
  VanityConfig ->
  -- | Progress callback (receives attempt count)
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchWithProgress config callback =
  let backend = vcBackend config
   in dispatchBackend backend
 where
  dispatchBackend CPU = CPU.searchVanityWithCallback config callback
#ifdef CUDA_ENABLED
  dispatchBackend CUDA = CUDA.searchVanityWithCallback config callback
#endif
#ifdef WGPU_ENABLED
  dispatchBackend WGPU = WGPU.searchVanityWithCallback config callback
#endif
  dispatchBackend b =
    error $
      "Backend not available: "
        ++ show b
        ++ ". Available backends: "
        ++ show availableBackends
