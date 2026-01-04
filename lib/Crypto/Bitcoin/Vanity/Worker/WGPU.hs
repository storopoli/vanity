{- |
Module: Crypto.Bitcoin.Vanity.Worker.WGPU
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

WGPU GPU backend for vanity address generation.
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
import Crypto.Bitcoin.Vanity.FFI.WGPU
import Crypto.Bitcoin.Vanity.Key (derivePublicKey)
import Crypto.Bitcoin.Vanity.Pattern (PatternError (..))
import Crypto.Bitcoin.Vanity.Types (
  Address (..),
  AddressType (..),
  Network (..),
  Pattern (..),
  SecretKey (..),
  VanityConfig (..),
  VanityResult (..),
 )
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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

-- | Convert VanityConfig to GPU config
toGPUConfig :: VanityConfig -> [Word8] -> VanityGPUConfig
toGPUConfig config patternBytes =
  VanityGPUConfig
    { cfgPattern = patternBytes
    , cfgPatternLen = fromIntegral (length patternBytes)
    , cfgAddressType = case vcAddressType config of
        P2PKH -> P2PKH_GPU
        P2WPKH -> P2WPKH_GPU
        P2TR -> P2TR_GPU
    , cfgNetwork = case vcNetwork config of
        Mainnet -> Mainnet_GPU
        Testnet -> Testnet_GPU
    , cfgBatchSize = batchSize
    , cfgMatchMode = 0 -- prefix match
    }

-- | Generate random 256-bit scalar
generateRandomScalar :: IO [Word32]
generateRandomScalar = replicateM 8 randomIO

-- | Convert 8 x Word32 (little-endian limbs) to Integer
word32sToInteger :: [Word32] -> Integer
word32sToInteger ws = foldr addLimb 0 (zip [0 ..] ws)
 where
  addLimb (i, w) acc = acc .|. (fromIntegral w `shiftL` (i * 32))

-- | Convert GPU result to VanityResult
fromGPUResult :: Word64 -> VanityGPUResult -> Maybe VanityResult
fromGPUResult attempts gpuResult =
  let scalar = word32sToInteger (resScalar gpuResult)
      secretKey = SecretKey scalar
      addressText = TE.decodeUtf8 $ BS.pack (resAddress gpuResult)
   in case derivePublicKey secretKey of
        Nothing -> Nothing
        Just publicKey ->
          Just
            VanityResult
              { vrSecretKey = secretKey
              , vrPublicKey = publicKey
              , vrAddress = Address addressText
              , vrAttempts = attempts
              }

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
    _ -> searchWithGPU config callback

searchWithGPU ::
  VanityConfig ->
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchWithGPU config callback = do
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
        Right name -> putStrLn $ "Using GPU: " ++ name
        Left _ -> pure ()

      -- Create compute pipeline with shader
      shaderCode <- readShaderFile
      pipelineResult <- wgpuCreatePipeline shaderCode
      case pipelineResult of
        Left err ->
          pure $
            Left $
              InvalidRegex $
                T.pack $
                  "Pipeline creation failed: " ++ show err
        Right () -> bracket (pure ()) (const wgpuDestroyPipeline) $ \_ ->
          searchLoop config callback

-- | Read the WGSL shader file
readShaderFile :: IO String
readShaderFile = do
  -- Try to read from the shaders directory
  result <- try $ readFile "shaders/vanity_pipeline.wgsl"
  case result of
    Right code -> pure code
    Left (_ :: SomeException) -> do
      -- Fall back to embedded minimal shader
      pure $
        unlines
          [ "// Placeholder shader - full shader not found"
          , "@group(0) @binding(0) var<uniform> config: Config;"
          , "@compute @workgroup_size(64)"
          , "fn main() {}"
          ]

-- | Main search loop
searchLoop ::
  VanityConfig ->
  (Word64 -> IO ()) ->
  IO (Either PatternError VanityResult)
searchLoop config callback = go 0
 where
  patternText = unPattern $ vcPattern config
  patternBytes = BS.unpack $ TE.encodeUtf8 patternText
  gpuConfig = toGPUConfig config patternBytes

  go :: Word64 -> IO (Either PatternError VanityResult)
  go totalAttempts = do
    -- Generate random base scalar for this batch
    baseScalar <- generateRandomScalar

    -- Launch GPU computation
    launchResult <- wgpuLaunchVanitySearch gpuConfig baseScalar
    case launchResult of
      Left err ->
        pure $
          Left $
            InvalidRegex $
              T.pack $
                "GPU launch failed: " ++ show err
      Right () -> do
        -- Get results
        results <- wgpuGetResults 64

        -- Update attempt count
        let newTotal = totalAttempts + fromIntegral batchSize
        callback newTotal

        -- Check for matches and find first valid result
        case mapMaybe (fromGPUResult newTotal) results of
          (result : _) -> pure $ Right result
          _ -> go newTotal -- No valid match, continue
