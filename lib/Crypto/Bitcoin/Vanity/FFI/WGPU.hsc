{- |
Module: Crypto.Bitcoin.Vanity.FFI.WGPU
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

FFI bindings to wgpu-native for cross-platform GPU compute.
Supports Metal (macOS), Vulkan (Linux/Windows), and DirectX (Windows).
-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Crypto.Bitcoin.Vanity.FFI.WGPU (
  -- * Types
  WGPUInstance,
  WGPUAdapter,
  WGPUDevice,
  WGPUQueue,
  WGPUBuffer,
  WGPUShaderModule,
  WGPUComputePipeline,
  WGPUBindGroup,
  WGPUBindGroupLayout,
  WGPUCommandEncoder,
  WGPUComputePassEncoder,
  WGPUCommandBuffer,

  -- * Configuration
  VanityGPUConfig (..),
  VanityGPUResult (..),
  AddressTypeGPU (..),
  NetworkGPU (..),

  -- * Initialization
  wgpuInit,
  wgpuCleanup,
  wgpuGetDeviceInfo,

  -- * Pipeline
  wgpuCreatePipeline,
  wgpuDestroyPipeline,

  -- * Execution
  wgpuLaunchVanitySearch,
  wgpuGetResults,
  wgpuGetAttemptCount,

  -- * Error handling
  WGPUError (..),
  getLastWGPUError,
) where

import Data.Word (Word32, Word64, Word8)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CChar, CInt (..), CSize (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peek, poke)

#include "wgpu_vanity.h"

-- | Opaque WGPU handles
data WGPUInstance
data WGPUAdapter
data WGPUDevice
data WGPUQueue
data WGPUBuffer
data WGPUShaderModule
data WGPUComputePipeline
data WGPUBindGroup
data WGPUBindGroupLayout
data WGPUCommandEncoder
data WGPUComputePassEncoder
data WGPUCommandBuffer

-- | Address type for GPU search
data AddressTypeGPU
  = P2PKH_GPU
  | P2WPKH_GPU
  | P2TR_GPU
  deriving (Eq, Show, Enum)

-- | Network for GPU search
data NetworkGPU
  = Mainnet_GPU
  | Testnet_GPU
  deriving (Eq, Show, Enum)

-- | GPU search configuration
data VanityGPUConfig = VanityGPUConfig
  { cfgPattern :: ![Word8]       -- ^ Pattern bytes to match (max 74)
  , cfgPatternLen :: !Word32     -- ^ Length of pattern
  , cfgAddressType :: !AddressTypeGPU
  , cfgNetwork :: !NetworkGPU
  , cfgBatchSize :: !Word32      -- ^ Keys per GPU dispatch (e.g., 1M)
  , cfgMatchMode :: !Word32      -- ^ 0=prefix, 1=suffix, 2=contains
  }

-- | Result from GPU search
data VanityGPUResult = VanityGPUResult
  { resScalar :: ![Word32]       -- ^ 256-bit scalar (8 x u32)
  , resPubkeyX :: ![Word32]      -- ^ X coordinate (8 x u32)
  , resPubkeyYParity :: !Word32  -- ^ 0 for even, 1 for odd
  , resAddress :: ![Word8]       -- ^ Address bytes
  , resAddressLen :: !Word32     -- ^ Length of address
  , resBatchIdx :: !Word32       -- ^ Index within batch
  }
  deriving (Eq, Show)

-- | WGPU error types
data WGPUError
  = WGPUErrorNone
  | WGPUErrorNoAdapter
  | WGPUErrorNoDevice
  | WGPUErrorShaderCompile
  | WGPUErrorPipelineCreate
  | WGPUErrorBufferCreate
  | WGPUErrorOutOfMemory
  | WGPUErrorUnknown
  deriving (Eq, Show, Enum)

-- | C structure for GPU config
data CWGPUConfig = CWGPUConfig
  { cPatternLen :: !CUInt
  , cAddressType :: !CUInt
  , cNetwork :: !CUInt
  , cBatchSize :: !CUInt
  , cMatchMode :: !CUInt
  }

instance Storable CWGPUConfig where
  sizeOf _ = #{size VanityWGPUConfig}
  alignment _ = #{alignment VanityWGPUConfig}
  peek ptr = do
    patLen <- #{peek VanityWGPUConfig, pattern_len} ptr
    addrType <- #{peek VanityWGPUConfig, address_type} ptr
    network <- #{peek VanityWGPUConfig, network} ptr
    batchSize <- #{peek VanityWGPUConfig, batch_size} ptr
    matchMode <- #{peek VanityWGPUConfig, match_mode} ptr
    return CWGPUConfig
      { cPatternLen = patLen
      , cAddressType = addrType
      , cNetwork = network
      , cBatchSize = batchSize
      , cMatchMode = matchMode
      }
  poke ptr cfg = do
    #{poke VanityWGPUConfig, pattern_len} ptr (cPatternLen cfg)
    #{poke VanityWGPUConfig, address_type} ptr (cAddressType cfg)
    #{poke VanityWGPUConfig, network} ptr (cNetwork cfg)
    #{poke VanityWGPUConfig, batch_size} ptr (cBatchSize cfg)
    #{poke VanityWGPUConfig, match_mode} ptr (cMatchMode cfg)

-- | C structure for GPU result
data CWGPUResult = CWGPUResult
  { cScalar :: ![Word32]
  , cPubkeyX :: ![Word32]
  , cPubkeyYParity :: !Word32
  , cAddress :: ![Word8]
  , cAddressLen :: !Word32
  , cBatchIdx :: !Word32
  , cFound :: !Word32
  }

instance Storable CWGPUResult where
  sizeOf _ = #{size VanityWGPUResult}
  alignment _ = #{alignment VanityWGPUResult}
  peek ptr = do
    scalar <- peekArray 8 (#{ptr VanityWGPUResult, scalar} ptr)
    pubkeyX <- peekArray 8 (#{ptr VanityWGPUResult, pubkey_x} ptr)
    yParity <- #{peek VanityWGPUResult, pubkey_y_parity} ptr
    address <- peekArray 76 (#{ptr VanityWGPUResult, address} ptr)
    addrLen <- #{peek VanityWGPUResult, address_len} ptr
    batchIdx <- #{peek VanityWGPUResult, batch_idx} ptr
    found <- #{peek VanityWGPUResult, found} ptr
    return CWGPUResult
      { cScalar = scalar
      , cPubkeyX = pubkeyX
      , cPubkeyYParity = yParity
      , cAddress = address
      , cAddressLen = addrLen
      , cBatchIdx = batchIdx
      , cFound = found
      }
  poke _ _ = error "CWGPUResult: poke not implemented"

-- | FFI imports
foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_init"
  c_wgpu_init :: IO CInt

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_cleanup"
  c_wgpu_cleanup :: IO ()

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_get_device_name"
  c_wgpu_get_device_name :: Ptr CChar -> CSize -> IO CInt

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_create_pipeline"
  c_wgpu_create_pipeline :: Ptr CChar -> CSize -> IO CInt

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_destroy_pipeline"
  c_wgpu_destroy_pipeline :: IO ()

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_launch"
  c_wgpu_launch :: Ptr CWGPUConfig -> Ptr Word8 -> Ptr Word32 -> IO CInt

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_get_results"
  c_wgpu_get_results :: Ptr CWGPUResult -> CUInt -> IO CUInt

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_get_attempt_count"
  c_wgpu_get_attempt_count :: IO Word64

foreign import capi unsafe "wgpu_vanity.h vanity_wgpu_get_last_error"
  c_wgpu_get_last_error :: IO CInt

-- | Initialize WGPU subsystem
wgpuInit :: IO (Either WGPUError ())
wgpuInit = do
  result <- c_wgpu_init
  if result == 0
    then return (Right ())
    else return (Left (toEnum (fromIntegral result)))

-- | Cleanup WGPU resources
wgpuCleanup :: IO ()
wgpuCleanup = c_wgpu_cleanup

-- | Get device info (name, type)
wgpuGetDeviceInfo :: IO (Either WGPUError String)
wgpuGetDeviceInfo = do
  allocaBytes 256 $ \buf -> do
    result <- c_wgpu_get_device_name buf 256
    if result == 0
      then do
        name <- peekCString buf
        return (Right name)
      else return (Left (toEnum (fromIntegral result)))

-- | Create compute pipeline from WGSL shader
wgpuCreatePipeline :: String -> IO (Either WGPUError ())
wgpuCreatePipeline shaderCode = do
  allocaBytes (length shaderCode + 1) $ \buf -> do
    pokeArray buf (map (fromIntegral . fromEnum) shaderCode ++ [0])
    result <- c_wgpu_create_pipeline buf (fromIntegral $ length shaderCode)
    if result == 0
      then return (Right ())
      else return (Left (toEnum (fromIntegral result)))

-- | Destroy compute pipeline
wgpuDestroyPipeline :: IO ()
wgpuDestroyPipeline = c_wgpu_destroy_pipeline

-- | Launch vanity address search on GPU
wgpuLaunchVanitySearch :: VanityGPUConfig -> [Word32] -> IO (Either WGPUError ())
wgpuLaunchVanitySearch config baseScalar = do
  let cConfig = CWGPUConfig
        { cPatternLen = fromIntegral (cfgPatternLen config)
        , cAddressType = fromIntegral (fromEnum (cfgAddressType config))
        , cNetwork = fromIntegral (fromEnum (cfgNetwork config))
        , cBatchSize = fromIntegral (cfgBatchSize config)
        , cMatchMode = fromIntegral (cfgMatchMode config)
        }
  allocaBytes 76 $ \patternBuf -> do
    pokeArray patternBuf (cfgPattern config ++ replicate (76 - length (cfgPattern config)) 0)
    allocaBytes 32 $ \scalarBuf -> do
      pokeArray scalarBuf (baseScalar ++ replicate (8 - length baseScalar) 0)
      alloca $ \configPtr -> do
        poke configPtr cConfig
        result <- c_wgpu_launch configPtr patternBuf scalarBuf
        if result == 0
          then return (Right ())
          else return (Left (toEnum (fromIntegral result)))

-- | Get results from last GPU search
wgpuGetResults :: Word32 -> IO [VanityGPUResult]
wgpuGetResults maxResults = do
  let bufSize = fromIntegral maxResults
  allocaBytes (bufSize * #{size VanityWGPUResult}) $ \buf -> do
    count <- c_wgpu_get_results buf (fromIntegral maxResults)
    if count > 0
      then do
        cResults <- peekArray (fromIntegral count) buf
        return $ map convertResult (filter ((> 0) . cFound) cResults)
      else return []
  where
    convertResult cr = VanityGPUResult
      { resScalar = cScalar cr
      , resPubkeyX = cPubkeyX cr
      , resPubkeyYParity = cPubkeyYParity cr
      , resAddress = take (fromIntegral (cAddressLen cr)) (cAddress cr)
      , resAddressLen = cAddressLen cr
      , resBatchIdx = cBatchIdx cr
      }

-- | Get total attempt count
wgpuGetAttemptCount :: IO Word64
wgpuGetAttemptCount = c_wgpu_get_attempt_count

-- | Get last error code
getLastWGPUError :: IO WGPUError
getLastWGPUError = do
  code <- c_wgpu_get_last_error
  return (toEnum (fromIntegral code))
