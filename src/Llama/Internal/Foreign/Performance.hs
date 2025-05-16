module Llama.Internal.Foreign.Performance (
 
  c_llama_perf_context
  , c_llama_perf_context_print
  , c_llama_perf_context_reset
  , c_llama_perf_sampler
  , c_llama_perf_sampler_print
  , c_llama_perf_sampler_reset

) where

import Llama.Internal.Types
import Foreign

-- | LLAMA_API struct llama_perf_context_data llama_perf_context(const struct llama_context * ctx);
foreign import ccall "llama_perf_context_into"
    c_llama_perf_context ::
        CLlamaContext ->  -- ctx
        Ptr LlamaPerfContextData -> -- output buffer
        IO ()

-- | LLAMA_API void llama_perf_context_print(const struct llama_context * ctx);
foreign import ccall "llama_perf_context_print"
  c_llama_perf_context_print ::
    CLlamaContext -> -- ctx
    IO ()

-- | LLAMA_API void llama_perf_context_reset(struct llama_context * ctx);
foreign import ccall "llama_perf_context_reset"
  c_llama_perf_context_reset ::
    CLlamaContext -> -- ctx
    IO ()

-- | LLAMA_API struct llama_perf_sampler_data llama_perf_sampler(const struct llama_sampler * chain);
foreign import ccall "llama_perf_sampler_into"
    c_llama_perf_sampler ::
        (Ptr LlamaSampler) ->  -- chain
        Ptr LlamaPerfSamplerData ->  -- output buffer
        IO ()

-- | LLAMA_API void llama_perf_sampler_print(const struct llama_sampler * chain);
foreign import ccall "llama_perf_sampler_print"
  c_llama_perf_sampler_print ::
    (Ptr LlamaSampler) -> -- chain
    IO ()

-- | LLAMA_API void llama_perf_sampler_reset(struct llama_sampler * chain);
foreign import ccall "llama_perf_sampler_reset"
  c_llama_perf_sampler_reset ::
    Ptr LlamaSampler -> -- chain
    IO ()
