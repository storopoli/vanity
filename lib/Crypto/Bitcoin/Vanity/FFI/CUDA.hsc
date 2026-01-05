{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module: Crypto.Bitcoin.Vanity.FFI.CUDA
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

FFI bindings for CUDA GPU backend.
-}
module Crypto.Bitcoin.Vanity.FFI.CUDA
  ( -- * Context Management
    GPUContext
  , cudaInit
  , cudaCleanup

    -- * Device Information
  , GPUInfo (..)
  , cudaGetInfo

    -- * Search Operations
  , SearchConfig (..)
  , MatchResult (..)
  , BatchResult (..)
  , cudaSearchBatch
  , cudaFreeResult

    -- * Error Codes
  , vanityOk
  , vanityErrInit
  , vanityErrMemory
  , vanityErrLaunch
  , vanityErrPattern
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word (Word32, Word64, Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (Storable (..))

#include "vanity_gpu.h"

-- | Error codes
vanityOk, vanityErrInit, vanityErrMemory, vanityErrLaunch, vanityErrPattern :: CInt
vanityOk = #{const VANITY_OK}
vanityErrInit = #{const VANITY_ERR_INIT}
vanityErrMemory = #{const VANITY_ERR_MEMORY}
vanityErrLaunch = #{const VANITY_ERR_LAUNCH}
vanityErrPattern = #{const VANITY_ERR_PATTERN}

-- | Address types
addrP2PKH, addrP2WPKH, addrP2TR :: Word8
addrP2PKH = #{const VANITY_ADDR_P2PKH}
addrP2WPKH = #{const VANITY_ADDR_P2WPKH}
addrP2TR = #{const VANITY_ADDR_P2TR}

-- | Network types
netMainnet, netTestnet :: Word8
netMainnet = #{const VANITY_NET_MAINNET}
netTestnet = #{const VANITY_NET_TESTNET}

-- | Opaque GPU context handle
data GPUContext

-- | GPU device information
data GPUInfo = GPUInfo
  { giName :: !Text
  , giMemoryTotal :: !Word64
  , giMemoryFree :: !Word64
  , giComputeCapability :: !Int
  , giMultiprocessors :: !Int
  }
  deriving (Show, Eq)

instance Storable GPUInfo where
  sizeOf _ = #{size vanity_gpu_info_t}
  alignment _ = #{alignment vanity_gpu_info_t}
  peek ptr = do
    name <- peekCString (castPtr ptr)
    memTotal <- #{peek vanity_gpu_info_t, memory_total} ptr
    memFree <- #{peek vanity_gpu_info_t, memory_free} ptr
    computeCap <- #{peek vanity_gpu_info_t, compute_capability} ptr
    multiProc <- #{peek vanity_gpu_info_t, multiprocessors} ptr
    pure GPUInfo
      { giName = T.pack name
      , giMemoryTotal = memTotal
      , giMemoryFree = memFree
      , giComputeCapability = computeCap
      , giMultiprocessors = multiProc
      }
  poke _ _ = error "GPUInfo poke not implemented"

-- | Search configuration
data SearchConfig = SearchConfig
  { scAddressType :: !Word8
  , scNetwork :: !Word8
  , scPattern :: !Text
  , scCaseSensitive :: !Bool
  , scBatchSize :: !Word32
  , scMaxResults :: !Word32
  }
  deriving (Show, Eq)

-- | Match result from GPU search
data MatchResult = MatchResult
  { mrScalar :: !ByteString    -- ^ 32-byte secret key
  , mrPubKey :: !ByteString    -- ^ 33-byte compressed public key
  , mrAddress :: !Text         -- ^ Bitcoin address string
  , mrBatchIdx :: !Word32      -- ^ Index within batch
  }
  deriving (Show, Eq)

instance Storable MatchResult where
  sizeOf _ = #{size vanity_match_t}
  alignment _ = #{alignment vanity_match_t}
  peek ptr = do
    scalar <- unsafePackCStringLen (castPtr ptr, 32)
    pubkey <- unsafePackCStringLen (castPtr ptr `plusPtr` 32, 33)
    addrLen <- #{peek vanity_match_t, address_len} ptr :: IO Word8
    address <- unsafePackCStringLen (castPtr ptr `plusPtr` 65, fromIntegral addrLen)
    batchIdx <- #{peek vanity_match_t, batch_idx} ptr
    pure MatchResult
      { mrScalar = scalar
      , mrPubKey = pubkey
      , mrAddress = T.decodeUtf8 address
      , mrBatchIdx = batchIdx
      }
  poke _ _ = error "MatchResult poke not implemented"

-- | Batch search result
data BatchResult = BatchResult
  { brMatches :: ![MatchResult]
  , brMatchCount :: !Word32
  , brKeysChecked :: !Word64
  }
  deriving (Show, Eq)

-- Foreign imports

foreign import capi "vanity_gpu.h vanity_cuda_init"
  c_vanity_cuda_init :: Ptr (Ptr GPUContext) -> CInt -> IO CInt

foreign import capi "vanity_gpu.h vanity_cuda_cleanup"
  c_vanity_cuda_cleanup :: Ptr GPUContext -> IO ()

foreign import capi "vanity_gpu.h vanity_cuda_get_info"
  c_vanity_cuda_get_info :: Ptr GPUContext -> Ptr GPUInfo -> IO CInt

-- | Initialize CUDA backend
cudaInit :: Maybe Int -> IO (Either CInt (Ptr GPUContext))
cudaInit mDeviceId = alloca $ \ctxPtr -> do
  let deviceId = maybe (-1) fromIntegral mDeviceId
  result <- c_vanity_cuda_init ctxPtr deviceId
  if result == vanityOk
    then Right <$> peek ctxPtr
    else pure (Left result)

-- | Cleanup CUDA backend
cudaCleanup :: Ptr GPUContext -> IO ()
cudaCleanup = c_vanity_cuda_cleanup

-- | Get CUDA device information
cudaGetInfo :: Ptr GPUContext -> IO (Either CInt GPUInfo)
cudaGetInfo ctx = alloca $ \infoPtr -> do
  result <- c_vanity_cuda_get_info ctx infoPtr
  if result == vanityOk
    then Right <$> peek infoPtr
    else pure (Left result)

-- | Perform a batch search
cudaSearchBatch :: Ptr GPUContext -> SearchConfig -> IO (Either CInt BatchResult)
cudaSearchBatch _ctx _config = do
  -- TODO: Implement full FFI binding
  -- This requires additional C wrapper functions
  pure $ Left vanityErrInit

-- | Free batch result resources
cudaFreeResult :: BatchResult -> IO ()
cudaFreeResult _ = pure ()  -- Memory managed by Haskell GC
