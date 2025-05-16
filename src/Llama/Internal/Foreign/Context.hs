module Llama.Internal.Foreign.Context
  ( llama_time_us
  , llama_max_devices
  , llama_supports_mmap
  , llama_supports_mlock
  , llama_supports_gpu_offload
  , llama_supports_rpc
  , llama_n_ctx
  , llama_n_batch
  , llama_n_ubatch
  , llama_n_seq_max
  , c_llama_pooling_type_into
  , c_llama_get_kv_self
  , c_llama_context_default_params_into
  , c_llama_detach_threadpool
  ) where

import Foreign
import Foreign.C
import Llama.Internal.Types
import Llama.Internal.Types.Params

-- LLAMA_API void llama_detach_threadpool(struct llama_context * ctx);
foreign import ccall "llama_detach_threadpool"
  c_llama_detach_threadpool :: CLlamaContext -> IO ()

foreign import ccall "llama_context_default_params_into"
  c_llama_context_default_params_into :: CLlamaContextParams -> IO ()

-- | Get current time in microseconds
foreign import ccall "llama_time_us"
  llama_time_us :: IO Int64

-- | Maximum number of devices supported
foreign import ccall "llama_max_devices"
  llama_max_devices :: IO CSize

-- | Features supported by backend
foreign import ccall "llama_supports_mmap"
  llama_supports_mmap :: IO CBool

foreign import ccall "llama_supports_mlock"
  llama_supports_mlock :: IO CBool

foreign import ccall "llama_supports_gpu_offload"
  llama_supports_gpu_offload :: IO CBool

foreign import ccall "llama_supports_rpc"
  llama_supports_rpc :: IO CBool

-- | Context-related accessors
foreign import ccall "llama_n_ctx"
  llama_n_ctx :: CLlamaContext -> IO CUInt

foreign import ccall "llama_n_batch"
  llama_n_batch :: CLlamaContext -> IO CUInt

foreign import ccall "llama_n_ubatch"
  llama_n_ubatch :: CLlamaContext -> IO CUInt

foreign import ccall "llama_n_seq_max"
  llama_n_seq_max :: CLlamaContext -> IO CUInt

-- | Get KV cache from context
foreign import ccall "llama_get_kv_self"
  c_llama_get_kv_self :: CLlamaContext -> IO CLlamaKVCache

-- | Get pooling type from context
foreign import ccall safe "llama_pooling_type_into"
  c_llama_pooling_type_into :: CLlamaContext -> Ptr CInt -> IO ()

-- llamaPoolingType :: CLlamaContext -> IO (Maybe LlamaPoolingType)
-- llamaPoolingType (CLlamaContext ctxPtr) =
--     alloca $ \outPtr -> do
--         c_llama_pooling_type_into ctxPtr outPtr
--         val <- peek outPtr
--         pure $ fromLlamaRopePoolingType val
