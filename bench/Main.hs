{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: Main
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Criterion benchmarks for the Bitcoin vanity address generator.
-}
module Main (main) where

import Control.DeepSeq (NFData (..), deepseq)
import Criterion.Main (
  Benchmark,
  bench,
  bgroup,
  defaultMain,
  env,
  nf,
  nfIO,
  whnfIO,
 )
import Crypto.Bitcoin.Vanity.Address (
  encodeAddress,
  encodeP2PKH,
  encodeP2TR,
  encodeP2WPKH,
 )
import Crypto.Bitcoin.Vanity.Key (
  derivePublicKey,
  generateSecretKey,
  serializePublicKey,
  serializePublicKeyXOnly,
 )
import Crypto.Bitcoin.Vanity.Pattern (
  compilePattern,
  matchPattern,
 )
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  AddressType (..),
  Network (..),
  Pattern (..),
  PublicKey (..),
  SecretKey (..),
 )
import Crypto.Curve.Secp256k1 (Projective)
import Data.Maybe (fromJust)

-- NFData instance for Projective (needed for benchmarking)
instance NFData Projective where
  rnf p = p `seq` ()

main :: IO ()
main =
  defaultMain
    [ keyBenchmarks
    , addressBenchmarks
    , patternBenchmarks
    , pipelineBenchmarks
    ]

-- | Key generation and derivation benchmarks.
keyBenchmarks :: Benchmark
keyBenchmarks =
  bgroup
    "Key Operations"
    [ bench "generateSecretKey" $ nfIO generateSecretKey
    , bench "derivePublicKey" $ nf derivePublicKey testSecretKey
    , bench "serializePublicKey (33 bytes)" $ nf serializePublicKey testPublicKey
    , bench "serializePublicKeyXOnly (32 bytes)" $ nf serializePublicKeyXOnly testPublicKey
    ]
 where
  !testSecretKey = SecretKey 0xB7E151628AED2A6ABF7158809CF4F3C762E7160F38B4DA56A784D9045190CFEF
  !testPublicKey = fromJust $ derivePublicKey testSecretKey

-- | Address encoding benchmarks.
addressBenchmarks :: Benchmark
addressBenchmarks =
  env setupPublicKey $ \pk ->
    bgroup
      "Address Encoding"
      [ bgroup
          "P2PKH (Base58Check)"
          [ bench "mainnet" $ nf (encodeP2PKH Mainnet) pk
          , bench "testnet" $ nf (encodeP2PKH Testnet) pk
          ]
      , bgroup
          "P2WPKH (Bech32)"
          [ bench "mainnet" $ nf (encodeP2WPKH Mainnet) pk
          , bench "testnet" $ nf (encodeP2WPKH Testnet) pk
          ]
      , bgroup
          "P2TR (Bech32m)"
          [ bench "mainnet" $ nf (encodeP2TR Mainnet) pk
          , bench "testnet" $ nf (encodeP2TR Testnet) pk
          ]
      ]
 where
  setupPublicKey = do
    let sk = SecretKey 0xB7E151628AED2A6ABF7158809CF4F3C762E7160F38B4DA56A784D9045190CFEF
    pure $! fromJust $ derivePublicKey sk

-- | Pattern matching benchmarks.
patternBenchmarks :: Benchmark
patternBenchmarks =
  env setupAddresses $ \addresses ->
    let Right !simplePat = compilePattern (Pattern "^1") True
        Right !complexPat = compilePattern (Pattern "^1[A-Z]{3}") True
     in bgroup
          "Pattern Matching"
          [ bench "compile simple pattern (^1)" $
              whnfIO $ pure $ compilePattern (Pattern "^1") True
          , bench "compile complex pattern (^1[A-Z]{3})" $
              whnfIO $ pure $ compilePattern (Pattern "^1[A-Z]{3}") True
          , bench "match simple pattern (1 address)" $
              nf (matchPattern simplePat) (head addresses)
          , bench "match complex pattern (1 address)" $
              nf (matchPattern complexPat) (head addresses)
          , bench "match simple pattern (100 addresses)" $
              nf (map (matchPattern simplePat)) addresses
          , bench "match complex pattern (100 addresses)" $
              nf (map (matchPattern complexPat)) addresses
          ]
 where
  setupAddresses = do
    -- Generate 100 P2PKH addresses
    addresses <- mapM generateAddress [1 .. 100 :: Int]
    addresses `deepseq` pure addresses

  generateAddress _ = do
    sk <- generateSecretKey
    let pk = fromJust $ derivePublicKey sk
    pure $ encodeP2PKH Mainnet pk

-- | Full pipeline benchmarks (key generation -> encoding -> matching).
pipelineBenchmarks :: Benchmark
pipelineBenchmarks =
  bgroup
    "Full Pipeline"
    [ bench "generate + derive + encode P2PKH" $
        nfIO $ do
          sk <- generateSecretKey
          let pk = fromJust $ derivePublicKey sk
          pure $ encodeP2PKH Mainnet pk
    , bench "generate + derive + encode P2WPKH" $
        nfIO $ do
          sk <- generateSecretKey
          let pk = fromJust $ derivePublicKey sk
          pure $ encodeP2WPKH Mainnet pk
    , bench "generate + derive + encode P2TR" $
        nfIO $ do
          sk <- generateSecretKey
          let pk = fromJust $ derivePublicKey sk
          pure $ encodeP2TR Mainnet pk
    , bench "generate + derive + encode + match P2PKH" $
        nfIO $ do
          let Right !pat = compilePattern (Pattern "^1") True
          sk <- generateSecretKey
          let pk = fromJust $ derivePublicKey sk
              addr = encodeP2PKH Mainnet pk
          pure $ matchPattern pat addr
    , bench "1000 iterations (P2PKH pipeline)" $
        nfIO $ do
          let Right !pat = compilePattern (Pattern "^1") True
          mapM_
            ( \_ -> do
                sk <- generateSecretKey
                let pk = fromJust $ derivePublicKey sk
                    addr = encodeP2PKH Mainnet pk
                pure $! matchPattern pat addr
            )
            [1 .. 1000 :: Int]
    ]
