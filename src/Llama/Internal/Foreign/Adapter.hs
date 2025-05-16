module Llama.Internal.Foreign.Adapter
  ( p_llama_adapter_lora_free
  , c_llama_adapter_lora_init
  , c_llama_adapter_lora_free
  , c_llama_set_adapter_lora
  , c_llama_rm_adapter_lora
  , c_llama_clear_adapter_lora
  , c_llama_apply_adapter_cvec
  ) where

import Foreign
import Foreign.C
import Llama.Internal.Types

{- | Foreign pointer to the C function for freeing LoRA adapters.
This allows Haskell's garbage collector to automatically free resources.
-}
foreign import ccall "&llama_adapter_lora_free"
  p_llama_adapter_lora_free :: FinalizerPtr CLlamaAdapterLora

{- | LLAMA_API struct llama_adapter_lora * llama_adapter_lora_init(
|             struct llama_model * model, const char * path_lora);
-}
foreign import ccall "llama_adapter_lora_init"
  c_llama_adapter_lora_init ::
    CLlamaModel -> -- model
    CString -> -- path_lora
    IO CLlamaAdapterLora

-- | LLAMA_API void llama_adapter_lora_free(struct llama_adapter_lora * adapter);
foreign import ccall "llama_adapter_lora_free"
  c_llama_adapter_lora_free ::
    CLlamaAdapterLora -> -- adapter
    IO ()

{- | LLAMA_API int32_t llama_set_adapter_lora(
|             struct llama_context * ctx,
|             struct llama_adapter_lora * adapter,
|             float scale);
-}
foreign import ccall "llama_set_adapter_lora"
  c_llama_set_adapter_lora ::
    CLlamaContext -> -- ctx
    CLlamaAdapterLora -> -- adapter
    CFloat -> -- scale
    IO CInt

{- | LLAMA_API int32_t llama_rm_adapter_lora(
|             struct llama_context * ctx,
|             struct llama_adapter_lora * adapter);
-}
foreign import ccall "llama_rm_adapter_lora"
  c_llama_rm_adapter_lora ::
    CLlamaContext -> -- ctx
    CLlamaAdapterLora -> -- adapter
    IO CInt

-- | LLAMA_API void llama_clear_adapter_lora(struct llama_context * ctx);
foreign import ccall "llama_clear_adapter_lora"
  c_llama_clear_adapter_lora ::
    CLlamaContext -> -- ctx
    IO ()

{- | LLAMA_API int32_t llama_apply_adapter_cvec(
|             struct llama_context * ctx,
|                    const float * data,
|                         size_t   len,
|                        int32_t   n_embd,
|                        int32_t   il_start,
|                        int32_t   il_end);
-}
foreign import ccall "llama_apply_adapter_cvec"
  c_llama_apply_adapter_cvec ::
    CLlamaContext -> -- ctx
    Ptr CFloat -> -- data (nullable)
    CSize -> -- len
    CInt -> -- n_embd
    CInt -> -- il_start
    CInt -> -- il_end
    IO CInt
