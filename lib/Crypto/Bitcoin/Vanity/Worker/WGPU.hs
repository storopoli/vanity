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

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (replicateM, unless, when)
import Crypto.Bitcoin.Vanity.FFI.WGPU
import Crypto.Bitcoin.Vanity.Pattern (PatternError (..))
import Crypto.Bitcoin.Vanity.Types (
  AddressType (..),
  Network (..),
  VanityConfig (..),
  VanityResult (..),
 )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import System.IO (hFlush, stdout)
import System.Random (randomIO)

-- | Batch size for GPU dispatch (number of keys per batch)
batchSize :: Word32
batchSize = 1024 * 1024 -- 1M keys per batch

-- | Read shader code from embedded data
getShaderCode :: IO String
getShaderCode = do
  -- In production, this would read from the installed data files
  -- For now, we embed a minimal test shader or read from file
  pure $
    unlines
      [ "// Minimal test shader"
      , "@compute @workgroup_size(64)"
      , "fn main() {}"
      ]

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

-- | Convert GPU result to VanityResult
fromGPUResult :: VanityGPUResult -> VanityResult
fromGPUResult gpuResult =
  VanityResult
    { vrSecretKeyWIF = T.pack "" -- Will be computed from scalar
    , vrSecretKeyHex = scalarToHex (resScalar gpuResult)
    , vrPublicKeyHex = pubkeyToHex (resPubkeyX gpuResult) (resPubkeyYParity gpuResult)
    , vrAddress = addressFromBytes (resAddress gpuResult)
    }
 where
  scalarToHex :: [Word32] -> T.Text
  scalarToHex ws = T.pack $ concatMap (printf "%08x") (reverse ws)

  pubkeyToHex :: [Word32] -> Word32 -> T.Text
  pubkeyToHex xs parity =
    let prefix = if parity == 0 then "02" else "03"
     in T.pack $ prefix ++ concatMap (printf "%08x") xs

  addressFromBytes :: [Word8] -> T.Text
  addressFromBytes = TE.decodeUtf8 . BS.pack

  printf :: String -> Word32 -> String
  printf _ w =
    let hex = "0123456789abcdef"
        toHex n =
          [ hex !! fromIntegral ((n `div` 16) `mod` 16)
          , hex !! fromIntegral (n `mod` 16)
          ]
        b0 = (w `div` 0x1000000) `mod` 256
        b1 = (w `div` 0x10000) `mod` 256
        b2 = (w `div` 0x100) `mod` 256
        b3 = w `mod` 256
     in concatMap (toHex . fromIntegral) [b0, b1, b2, b3]

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
  patternText = vcPattern config
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

        -- Check for matches
        case results of
          [] -> go newTotal -- No match, continue
          (match : _) -> do
            -- Found a match!
            pure $ Right $ fromGPUResult match
