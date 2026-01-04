{- |
Module: Crypto.Bitcoin.Vanity.Worker.WGPU
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

WGPU GPU backend for vanity address generation using hybrid approach.
GPU computes EC scalar multiplication (the bottleneck), CPU does
hashing, encoding, and pattern matching.

Provides cross-platform GPU support via Metal (macOS), Vulkan (Linux), or DirectX (Windows).
-}
module Crypto.Bitcoin.Vanity.Worker.WGPU (
  searchVanityWithCallback,

  -- * Device info
  getGPUDeviceName,
  isWGPUAvailable,
) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (replicateM)
import Crypto.Bitcoin.Vanity.Address.Bech32 (bech32HRP, prependWitnessVersion)
import Crypto.Bitcoin.Vanity.Address.Bech32m (bech32mHRP)
import Crypto.Bitcoin.Vanity.FFI.WGPU
import Crypto.Bitcoin.Vanity.Pattern (CompiledPattern, PatternError (..), compilePattern, matchPattern)
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  AddressType (..),
  Network (..),
  PublicKey (..),
  SecretKey (..),
  VanityConfig (..),
  VanityResult (..),
 )
import Crypto.Curve.Secp256k1 (parse_point)
import Crypto.Hash.RIPEMD160 qualified as RIPEMD160
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Bech32 qualified as Bech32
import Data.ByteString.Bech32m qualified as Bech32m
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32, Word64, Word8)
import System.Random (randomIO)

-- | Batch size for GPU dispatch (number of keys per batch)
batchSize :: Word32
batchSize = 1024 * 1024 -- 1M keys per batch

-- | Check if WGPU backend is available
isWGPUAvailable :: IO Bool
isWGPUAvailable = do
  result <- wgpuInit
  case result of
    Right () -> do
      wgpuCleanup
      pure True
    Left _ -> pure False

-- | Get GPU device name
getGPUDeviceName :: IO (Maybe String)
getGPUDeviceName = do
  initResult <- wgpuInit
  case initResult of
    Left _ -> pure Nothing
    Right () -> do
      nameResult <- wgpuGetDeviceInfo
      wgpuCleanup
      case nameResult of
        Left _ -> pure Nothing
        Right name -> pure (Just name)

-- | Generate random 256-bit scalar
generateRandomScalar :: IO [Word32]
generateRandomScalar = replicateM 8 randomIO

-- | Convert 8 x Word32 (little-endian limbs) to Integer
word32sToInteger :: [Word32] -> Integer
word32sToInteger ws = foldr addLimb 0 (zip [0 ..] ws)
 where
  addLimb (i, w) acc = acc .|. (fromIntegral w `shiftL` (i * 32))

{- | Convert 8 x Word32 (big-endian packed bytes) to 32 bytes.
GPU stores X coordinate as big-endian bytes packed into u32s:
  u32[0] contains bytes[0..3], u32[1] contains bytes[4..7], etc.
On little-endian host, we need to byte-swap each u32.
-}
word32sToBytesSwapped :: [Word32] -> ByteString
word32sToBytesSwapped ws = BS.pack $ concatMap word32ToBytesBE ws
 where
  -- Convert Word32 to 4 bytes in big-endian order
  word32ToBytesBE :: Word32 -> [Word8]
  word32ToBytesBE w =
    [ fromIntegral (w `shiftR` 24) .&. 0xff
    , fromIntegral (w `shiftR` 16) .&. 0xff
    , fromIntegral (w `shiftR` 8) .&. 0xff
    , fromIntegral w .&. 0xff
    ]

{- | Create compressed public key bytes from GPU result.
Returns 33 bytes: prefix (0x02 for even Y, 0x03 for odd Y) + X coordinate.
-}
makeCompressedPubKey :: [Word32] -> Word32 -> ByteString
makeCompressedPubKey xCoord yParity =
  let prefix = if yParity == 0 then 0x02 else 0x03
      xBytes = word32sToBytesSwapped xCoord
   in BS.cons prefix xBytes

-- | Encode P2WPKH address from compressed public key bytes
encodeP2WPKHFromBytes :: Network -> ByteString -> Address
encodeP2WPKHFromBytes network pubKeyBytes =
  let sha256Hash = SHA256.hash pubKeyBytes
      hash160 = RIPEMD160.hash sha256Hash
      witnessData = prependWitnessVersion 0 hash160
      hrp = bech32HRP network
      encoded = case Bech32.encode hrp witnessData of
        Just bs -> bs
        Nothing -> error "Bech32 encoding failed"
   in Address (decodeUtf8 encoded)

-- | Encode P2TR address from X-only public key (32 bytes)
encodeP2TRFromXOnly :: Network -> ByteString -> Address
encodeP2TRFromXOnly network xOnlyPubKey =
  let witnessData = prependWitnessVersion 1 xOnlyPubKey
      hrp = bech32mHRP network
      encoded = case Bech32m.encode hrp witnessData of
        Just bs -> bs
        Nothing -> error "Bech32m encoding failed"
   in Address (decodeUtf8 encoded)

{- | Convert GPU result to VanityResult if pattern matches.
Returns Nothing if pattern doesn't match or public key parsing fails.
-}
fromHybridResult ::
  VanityConfig ->
  CompiledPattern ->
  Word64 ->
  PubKeyResult ->
  Maybe VanityResult
fromHybridResult config compiledPat attempts pkResult =
  let scalar = word32sToInteger (pkScalar pkResult)
      secretKey = SecretKey scalar
      xBytes = word32sToBytesSwapped (pkPubkeyX pkResult)
      yParity = pkPubkeyYParity pkResult
      compressedPubKey = makeCompressedPubKey (pkPubkeyX pkResult) yParity

      -- Encode address based on address type
      address = case vcAddressType config of
        P2WPKH -> encodeP2WPKHFromBytes (vcNetwork config) compressedPubKey
        P2TR -> encodeP2TRFromXOnly (vcNetwork config) xBytes
        P2PKH -> error "P2PKH not supported in GPU backend"
   in -- Check if pattern matches
      if matchPattern compiledPat address
        then -- Parse the compressed pubkey to get a proper PublicKey
          case parse_point compressedPubKey of
            Just proj ->
              Just
                VanityResult
                  { vrSecretKey = secretKey
                  , vrPublicKey = PublicKey proj
                  , vrAddress = address
                  , vrAttempts = attempts
                  }
            Nothing -> Nothing -- Should not happen with valid GPU output
        else Nothing -- Pattern doesn't match

-- | Search for a vanity address using WGPU
searchVanityWithCallback ::
  VanityConfig ->
  -- | Progress callback (receives attempt count)
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchVanityWithCallback config callback = do
  -- Check if P2PKH (not supported on GPU due to Base58 complexity)
  case vcAddressType config of
    P2PKH ->
      pure $
        Left $
          InvalidRegex $
            T.pack "P2PKH addresses not supported on GPU backend (use CPU or P2WPKH/P2TR)"
    _ -> do
      -- Compile pattern first (before GPU init)
      case compilePattern (vcPattern config) (vcCaseSensitive config) of
        Left err -> pure $ Left err
        Right compiledPat -> searchWithGPU config compiledPat callback

searchWithGPU ::
  VanityConfig ->
  CompiledPattern ->
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchWithGPU config compiledPat callback = do
  -- Initialize WGPU
  initResult <- wgpuInit
  case initResult of
    Left err ->
      pure $
        Left $
          InvalidRegex $
            T.pack $
              "WGPU initialization failed: " ++ show err
    Right () -> bracket (pure ()) (const wgpuCleanup) $ \_ -> do
      -- Get device info for logging
      deviceInfo <- wgpuGetDeviceInfo
      case deviceInfo of
        Right name -> putStrLn $ "Using GPU (hybrid): " ++ name
        Left _ -> pure ()

      -- Create hybrid compute pipeline with EC-only shader
      shaderCode <- readHybridShaderFile
      pipelineResult <- wgpuCreateHybridPipeline shaderCode
      case pipelineResult of
        Left err ->
          pure $
            Left $
              InvalidRegex $
                T.pack $
                  "Hybrid pipeline creation failed: " ++ show err
        Right () -> bracket (pure ()) (const wgpuDestroyPipeline) $ \_ ->
          searchLoop config compiledPat callback

-- | Read the hybrid WGSL shader file
readHybridShaderFile :: IO String
readHybridShaderFile = do
  -- Try to read from the shaders directory
  result <- try $ readFile "shaders/ecmult_hybrid.wgsl"
  case result of
    Right code -> pure code
    Left (_ :: SomeException) -> do
      -- Fall back to embedded minimal shader
      pure $
        unlines
          [ "// Placeholder shader - hybrid shader not found"
          , "@group(0) @binding(0) var<uniform> config: Config;"
          , "@compute @workgroup_size(64)"
          , "fn main() {}"
          ]

-- | Main search loop (hybrid approach)
searchLoop ::
  VanityConfig ->
  CompiledPattern ->
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchLoop config compiledPat callback = go 0
 where
  hybridConfig = HybridConfig{hcBatchSize = batchSize}

  go :: Word64 -> IO (Either PatternError VanityResult)
  go totalAttempts = do
    -- Generate random base scalar for this batch
    baseScalar <- generateRandomScalar

    -- Launch hybrid GPU computation (EC only)
    launchResult <- wgpuLaunchHybrid hybridConfig baseScalar
    case launchResult of
      Left err ->
        pure $
          Left $
            InvalidRegex $
              T.pack $
                "GPU launch failed: " ++ show err
      Right () -> do
        -- Get public key results from GPU
        pubKeyResults <- wgpuGetPubKeys batchSize

        -- Update attempt count
        let newTotal = totalAttempts + fromIntegral batchSize
        callback newTotal

        -- CPU-side: encode addresses and match patterns
        case findMatch pubKeyResults of
          Just result -> pure $ Right result
          Nothing -> go newTotal -- No match, continue
   where
    -- Find first result that matches the pattern
    findMatch :: [PubKeyResult] -> Maybe VanityResult
    findMatch [] = Nothing
    findMatch (pk : pks) =
      case fromHybridResult config compiledPat (totalAttempts + 1) pk of
        Just result -> Just result
        Nothing -> findMatch pks
