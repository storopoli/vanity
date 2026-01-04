{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Crypto.Bitcoin.Vanity.Worker.CPU
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Multi-threaded CPU backend for vanity address generation.

This module implements a parallel worker pool that generates and tests
Bitcoin addresses across multiple CPU cores.
-}
module Crypto.Bitcoin.Vanity.Worker.CPU (
  -- * Search Functions
  searchVanity,
  searchVanityWithCallback,
) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM (
  TVar,
  atomically,
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
  writeTVar,
 )
import Control.Monad (when)
import Crypto.Bitcoin.Vanity.Address (encodeAddress)
import Crypto.Bitcoin.Vanity.Key (derivePublicKey, generateSecretKey)
import Crypto.Bitcoin.Vanity.Pattern (
  CompiledPattern,
  PatternError,
  compilePattern,
  matchPattern,
 )
import Crypto.Bitcoin.Vanity.Types (
  AddressType,
  Network,
  VanityConfig (..),
  VanityResult (..),
 )
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import GHC.Conc (forkIO)

{- | Search for a vanity address matching the configured pattern.

This is the main entry point for CPU-based vanity address generation.
It spawns multiple worker threads (one per CPU core by default) and
returns as soon as any worker finds a matching address.
-}
searchVanity :: VanityConfig -> IO (Either PatternError VanityResult)
searchVanity config = searchVanityWithCallback config (const $ pure ())

{- | Search for a vanity address with a progress callback.

The callback is invoked periodically with the current attempt count.
This allows displaying progress to the user.
-}
searchVanityWithCallback ::
  VanityConfig ->
  -- | Progress callback (receives attempt count)
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchVanityWithCallback VanityConfig{..} callback = do
  case compilePattern vcPattern vcCaseSensitive of
    Left err -> pure (Left err)
    Right compiled -> do
      -- Determine thread count
      numCaps <- getNumCapabilities
      let !numThreads = fromMaybe numCaps vcThreads

      -- Shared state for result and attempt counter
      resultVar <- newTVarIO Nothing
      attemptCounter <- newIORef (0 :: Word64)

      -- Spawn worker threads
      let worker = workerLoop compiled vcNetwork vcAddressType resultVar attemptCounter callback

      -- Fork workers
      mapM_ (\_ -> forkIO worker) [1 .. numThreads]

      -- Wait for result
      result <- atomically $ do
        mResult <- readTVar resultVar
        maybe retry pure mResult

      pure (Right result)

-- | Single worker loop that generates keys and checks for matches.
workerLoop ::
  CompiledPattern ->
  Network ->
  AddressType ->
  TVar (Maybe VanityResult) ->
  IORef Word64 ->
  (Word64 -> IO ()) ->
  IO ()
workerLoop compiled network addrType resultVar counter callback = go 0
 where
  go !localAttempts = do
    -- Check if another worker found a result
    maybeResult <- readTVarIO resultVar
    case maybeResult of
      Just _ -> pure () -- Another worker found it, exit
      Nothing -> do
        -- Generate a new key pair
        sk <- generateSecretKey
        case derivePublicKey sk of
          Nothing -> go localAttempts -- Invalid key, retry (should never happen)
          Just pk -> do
            let !addr = encodeAddress network addrType pk

            if matchPattern compiled addr
              then do
                -- Found a match!
                let !totalAttempts = localAttempts + 1
                atomically $
                  writeTVar resultVar $
                    Just
                      VanityResult
                        { vrSecretKey = sk
                        , vrPublicKey = pk
                        , vrAddress = addr
                        , vrAttempts = totalAttempts
                        }
              else do
                -- Update counter and continue
                let !newLocal = localAttempts + 1

                -- Report progress every 10000 attempts
                when (newLocal `mod` 10000 == 0) $ do
                  total <- atomicModifyIORef' counter (\c -> let c' = c + 10000 in (c', c'))
                  callback total

                go newLocal
