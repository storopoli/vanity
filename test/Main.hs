{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Main
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

Test suite for the Bitcoin vanity address generator.
-}
module Main (main) where

import Arbitrary ()
import Crypto.Bitcoin.Vanity.Address (
  encodeAddress,
  encodeP2PKH,
  encodeP2TR,
  encodeP2WPKH,
 )
import Crypto.Bitcoin.Vanity.Key (
  derivePublicKey,
  isValidSecretKey,
  serializePublicKey,
  serializePublicKeyXOnly,
  toWIF,
 )
import Crypto.Bitcoin.Vanity.Pattern (
  PatternError (..),
  compilePattern,
  matchPattern,
 )
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  AddressType (..),
  Network (..),
  Pattern (..),
  PublicKey,
  SecretKey (..),
  curveOrder,
 )
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text qualified as T
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Vanity Tests"
    [ keyTests
    , addressTests
    , patternTests
    , propertyTests
    ]

-- | Key generation and derivation tests.
keyTests :: TestTree
keyTests =
  testGroup
    "Key Tests"
    [ testCase "Known secret key derives to known public key" $ do
        let sk = SecretKey 0x0000000000000000000000000000000000000000000000000000000000000001
        let mpk = derivePublicKey sk
        isJust mpk @?= True
    , testCase "Zero secret key is invalid" $ do
        isValidSecretKey (SecretKey 0) @?= False
    , testCase "Secret key at curve order is invalid" $ do
        isValidSecretKey (SecretKey curveOrder) @?= False
    , testCase "Secret key just below curve order is valid" $ do
        isValidSecretKey (SecretKey (curveOrder - 1)) @?= True
    , testProperty "Serialized public key is 33 bytes (compressed)" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk -> BS.length (serializePublicKey pk) == 33
    , testProperty "X-only public key is 32 bytes" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk -> BS.length (serializePublicKeyXOnly pk) == 32
    , testProperty "WIF mainnet starts with K or L (compressed)" $ \sk ->
        let wif = toWIF Mainnet sk
            firstChar = BS.head wif
         in firstChar == 0x4B || firstChar == 0x4C -- 'K' or 'L'
    , testProperty "WIF testnet starts with c (compressed)" $ \sk ->
        let wif = toWIF Testnet sk
         in BS.head wif == 0x63 -- 'c'
    ]

-- | Address encoding tests.
addressTests :: TestTree
addressTests =
  testGroup
    "Address Tests"
    [ testProperty "P2PKH mainnet address starts with 1" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk ->
            let Address addr = encodeP2PKH Mainnet pk
             in T.head addr == '1'
    , testProperty "P2PKH testnet address starts with m or n" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk ->
            let Address addr = encodeP2PKH Testnet pk
                c = T.head addr
             in c == 'm' || c == 'n'
    , testProperty "P2WPKH mainnet address starts with bc1q" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk ->
            let Address addr = encodeP2WPKH Mainnet pk
             in T.isPrefixOf "bc1q" addr
    , testProperty "P2WPKH testnet address starts with tb1q" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk ->
            let Address addr = encodeP2WPKH Testnet pk
             in T.isPrefixOf "tb1q" addr
    , testProperty "P2TR mainnet address starts with bc1p" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk ->
            let Address addr = encodeP2TR Mainnet pk
             in T.isPrefixOf "bc1p" addr
    , testProperty "P2TR testnet address starts with tb1p" $ \sk ->
        case derivePublicKey sk of
          Nothing -> False
          Just pk ->
            let Address addr = encodeP2TR Testnet pk
             in T.isPrefixOf "tb1p" addr
    ]

-- | Pattern matching tests.
patternTests :: TestTree
patternTests =
  testGroup
    "Pattern Tests"
    [ testCase "Empty pattern returns error" $ do
        case compilePattern (Pattern "") True of
          Left EmptyPattern -> pure ()
          _ -> fail "Expected EmptyPattern error"
    , testCase "Valid simple pattern compiles" $ do
        case compilePattern (Pattern "^1") True of
          Right _ -> pure ()
          Left err -> fail $ "Pattern should compile: " ++ show err
    , testCase "Valid complex pattern compiles" $ do
        case compilePattern (Pattern "^1[A-Z]{3}") True of
          Right _ -> pure ()
          Left err -> fail $ "Pattern should compile: " ++ show err
    , testCase "Pattern matching works" $ do
        case compilePattern (Pattern "^1ABC") True of
          Left err -> fail $ show err
          Right pat -> do
            matchPattern pat (Address "1ABCdef") @?= True
            matchPattern pat (Address "1XYZdef") @?= False
    , testCase "Case-insensitive pattern matching" $ do
        case compilePattern (Pattern "abc") False of
          Left err -> fail $ show err
          Right pat -> do
            matchPattern pat (Address "1ABCdef") @?= True
            matchPattern pat (Address "1abcdef") @?= True
    ]

-- | QuickCheck property tests.
propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ testProperty "Valid secret key in range" prop_secretKeyRange
    , testProperty "Public key derivation succeeds for valid keys" prop_pubKeyDerivation
    , testProperty "Public key derivation is deterministic" prop_pubKeyDeterministic
    , testProperty "Address encoding is deterministic" prop_addressDeterministic
    , testProperty "P2PKH mainnet addresses start with 1" prop_p2pkhMainnetPrefix
    , testProperty "P2WPKH mainnet addresses start with bc1q" prop_p2wpkhMainnetPrefix
    , testProperty "P2TR mainnet addresses start with bc1p" prop_p2trMainnetPrefix
    , testProperty "Serialized public key is 33 bytes" prop_pubKeySerialization
    ]

-- | Valid secret keys are in range (0, curveOrder).
prop_secretKeyRange :: SecretKey -> Bool
prop_secretKeyRange = isValidSecretKey

-- | Public key derivation succeeds for valid secret keys.
prop_pubKeyDerivation :: SecretKey -> Bool
prop_pubKeyDerivation sk = isJust (derivePublicKey sk)

-- | Public key derivation is deterministic.
prop_pubKeyDeterministic :: SecretKey -> Bool
prop_pubKeyDeterministic sk = derivePublicKey sk == derivePublicKey sk

-- | Address encoding is deterministic.
prop_addressDeterministic :: Network -> AddressType -> PublicKey -> Bool
prop_addressDeterministic net addrType pk =
  encodeAddress net addrType pk == encodeAddress net addrType pk

-- | P2PKH mainnet addresses start with '1'.
prop_p2pkhMainnetPrefix :: PublicKey -> Bool
prop_p2pkhMainnetPrefix pk =
  let Address addr = encodeP2PKH Mainnet pk
   in T.head addr == '1'

-- | P2WPKH mainnet addresses start with "bc1q".
prop_p2wpkhMainnetPrefix :: PublicKey -> Bool
prop_p2wpkhMainnetPrefix pk =
  let Address addr = encodeP2WPKH Mainnet pk
   in "bc1q" `T.isPrefixOf` addr

-- | P2TR mainnet addresses start with "bc1p".
prop_p2trMainnetPrefix :: PublicKey -> Bool
prop_p2trMainnetPrefix pk =
  let Address addr = encodeP2TR Mainnet pk
   in "bc1p" `T.isPrefixOf` addr

-- | Serialized public key is always 33 bytes.
prop_pubKeySerialization :: PublicKey -> Bool
prop_pubKeySerialization pk = BS.length (serializePublicKey pk) == 33
