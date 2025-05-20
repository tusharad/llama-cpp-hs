{- |
Module      : Llama.Context
Description : High level context interface for llama-cpp
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-}
module Llama.Context (
    supportsRpc
, supportsGpuOffload
, supportsMLock
, supportsMMap
, getMaxDevices
, getTimeUs
, getContextSize
, getBatchSize
, getUnbatchedSize
, getMaxSeqCount
, getPoolingType
, detachThreadPool
, defaultContextParams
) where

import Llama.Internal.Foreign
import Llama.Internal.Types
import Llama.Internal.Types.Params
import Foreign

-- | Check if the backend supports remote procedure calls (RPC).
supportsRpc :: IO Bool
supportsRpc = toBool <$> llama_supports_rpc

-- | Check if the backend supports GPU offloading.
supportsGpuOffload :: IO Bool
supportsGpuOffload = toBool <$> llama_supports_gpu_offload

-- | Check if the backend supports locking model memory into RAM (no swapping).
supportsMLock :: IO Bool
supportsMLock = toBool <$> llama_supports_mlock

-- | Check if the backend supports memory mapping models.
supportsMMap :: IO Bool
supportsMMap = toBool <$> llama_supports_mmap

-- | Get maximum number of devices supported by the backend (e.g., GPUs).
getMaxDevices :: IO Int
getMaxDevices = fromIntegral <$> llama_max_devices

-- | Get current time in microseconds since some unspecified epoch.
getTimeUs :: IO Int
getTimeUs = fromIntegral <$> llama_time_us

-- | Get the maximum context size (n_ctx) of the model in the given context.
getContextSize :: Context -> IO Int
getContextSize (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> llama_n_ctx (CLlamaContext ctxPtr)

-- | Get the batch size (n_batch) used by the context.
getBatchSize :: Context -> IO Int
getBatchSize (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> llama_n_batch (CLlamaContext ctxPtr)

-- | Get the unbatched size (n_ubatch).
getUnbatchedSize :: Context -> IO Int
getUnbatchedSize (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> llama_n_ubatch (CLlamaContext ctxPtr)

-- | Get the maximum number of sequences supported.
getMaxSeqCount :: Context -> IO Int
getMaxSeqCount (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> llama_n_seq_max (CLlamaContext ctxPtr)

-- | Get the pooling type used by the context.
getPoolingType :: Context -> IO (Maybe LlamaPoolingType)
getPoolingType (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr -> alloca $ \outPtr -> do
    c_llama_pooling_type_into (CLlamaContext ctxPtr) outPtr
    val <- peek outPtr
    case val of
      -1 -> return Nothing
      _  -> return $ fromLlamaRopePoolingType val

-- | Detach the internal threadpool from the context.
detachThreadPool :: Context -> IO ()
detachThreadPool (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_detach_threadpool (CLlamaContext ctxPtr)

-- | Allocate and initialize a new 'LlamaContextParams' with defaults.
defaultContextParams :: IO LlamaContextParams
defaultContextParams = do
  alloca $ \ptr -> do
    c_llama_context_default_params_into (CLlamaContextParams ptr)
    peek ptr
