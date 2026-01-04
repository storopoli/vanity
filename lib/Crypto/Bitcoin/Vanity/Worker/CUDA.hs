{-# LANGUAGE LambdaCase #-}

{- |
Module: Crypto.Bitcoin.Vanity.Worker.CUDA
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

CUDA GPU backend for vanity address generation.
-}
module Crypto.Bitcoin.Vanity.Worker.CUDA (
  searchVanityWithCallback,
) where

import Crypto.Bitcoin.Vanity.FFI.CUDA qualified as FFI
import Crypto.Bitcoin.Vanity.Key (derivePublicKey)
import Crypto.Bitcoin.Vanity.Pattern (PatternError (..), compilePattern)
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  AddressType (..),
  Backend (..),
  Network (..),
  Pattern (..),
  PublicKey (..),
  SecretKey (..),
  VanityConfig (..),
  VanityResult (..),
 )
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import Foreign.Ptr (nullPtr)

-- | Search for a vanity address using CUDA GPU.
searchVanityWithCallback ::
  VanityConfig ->
  -- | Progress callback (receives attempt count)
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchVanityWithCallback config callback = do
  -- Compile pattern first (validation)
  case compilePattern (vcPattern config) (vcCaseSensitive config) of
    Left err -> pure (Left err)
    Right _compiled -> do
      -- Initialize CUDA
      FFI.cudaInit Nothing >>= \case
        Left errCode -> do
          pure $
            Left $
              InvalidRegex $
                T.pack $
                  "CUDA initialization failed: " ++ show errCode
        Right ctx -> do
          -- Main search loop
          result <- searchLoop ctx config callback 0

          -- Cleanup
          FFI.cudaCleanup ctx

          pure result

-- | Main search loop
searchLoop ::
  FFI.Ptr FFI.GPUContext ->
  VanityConfig ->
  (Word64 -> IO ()) ->
  Word64 ->
  IO (Either PatternError VanityResult)
searchLoop ctx config callback totalAttempts = do
  -- Create search config
  let searchConfig =
        FFI.SearchConfig
          { FFI.scAddressType = addressTypeToWord8 (vcAddressType config)
          , FFI.scNetwork = networkToWord8 (vcNetwork config)
          , FFI.scPattern = unPattern (vcPattern config)
          , FFI.scCaseSensitive = vcCaseSensitive config
          , FFI.scBatchSize = 1048576 -- 1M keys per batch
          , FFI.scMaxResults = 1
          }

  -- Run batch search
  FFI.cudaSearchBatch ctx searchConfig >>= \case
    Left errCode -> do
      pure $
        Left $
          InvalidRegex $
            T.pack $
              "CUDA search failed: " ++ show errCode
    Right batchResult -> do
      let newTotal = totalAttempts + fromIntegral (FFI.brKeysChecked batchResult)

      -- Report progress
      callback newTotal

      -- Check for matches
      case FFI.brMatches batchResult of
        (match : _) -> do
          -- Convert match to VanityResult
          let sk = bytesToSecretKey (FFI.mrScalar match)
          case derivePublicKey sk of
            Nothing -> searchLoop ctx config callback newTotal
            Just pk ->
              pure $
                Right
                  VanityResult
                    { vrSecretKey = sk
                    , vrPublicKey = pk
                    , vrAddress = Address (FFI.mrAddress match)
                    , vrAttempts = newTotal
                    }
        [] -> searchLoop ctx config callback newTotal

-- | Convert AddressType to Word8
addressTypeToWord8 :: AddressType -> Word8
addressTypeToWord8 P2PKH = 0
addressTypeToWord8 P2WPKH = 1
addressTypeToWord8 P2TR = 2

-- | Convert Network to Word8
networkToWord8 :: Network -> Word8
networkToWord8 Mainnet = 0
networkToWord8 Testnet = 1

-- | Convert 32-byte ByteString to SecretKey
bytesToSecretKey :: BS.ByteString -> SecretKey
bytesToSecretKey bs =
  let bytes = BS.unpack bs
      toInteger256 = foldl (\acc b -> acc * 256 + fromIntegral b) 0
   in SecretKey (toInteger256 bytes)
