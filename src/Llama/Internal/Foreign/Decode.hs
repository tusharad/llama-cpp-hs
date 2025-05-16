module Llama.Internal.Foreign.Decode (
  c_llama_batch_get_one_into
 , c_llama_batch_init_into
 , c_llama_batch_free_wrap
 , c_llama_encode
 , c_llama_decode_wrap
 , c_llama_set_n_threads
 , c_llama_n_threads
 , c_llama_n_threads_batch
 , c_llama_set_embeddings
 , c_llama_set_causal_attn
 , c_llama_set_warmup
 , c_llama_set_abort_callback
 , c_llama_synchronize
 , c_llama_get_logits
 , c_llama_get_logits_ith
 , c_llama_get_embeddings
 , c_llama_get_embeddings_ith
 , c_llama_get_embeddings_seq
) where

import Foreign
import Foreign.C
import Llama.Internal.Types

-- | LLAMA_API struct llama_batch llama_batch_get_one(llama_token * tokens, int32_t n_tokens);
foreign import ccall "llama_batch_get_one_into"
  c_llama_batch_get_one_into ::
    Ptr LlamaToken -> -- tokens
    CInt -> -- n_tokens
    Ptr LlamaBatch -> -- out_batch
    IO ()

-- | LLAMA_API struct llama_batch llama_batch_init(int32_t n_tokens, int32_t embd, int32_t n_seq_max);
foreign import ccall "llama_batch_init_into"
  c_llama_batch_init_into ::
    CInt -> -- n_tokens
    CInt -> -- embd
    CInt -> -- n_seq_max
    Ptr LlamaBatch -> -- out
    IO ()

-- | LLAMA_API void llama_batch_free(struct llama_batch batch);
foreign import ccall "llama_batch_free_wrap"
  c_llama_batch_free_wrap :: Ptr LlamaBatch -> IO ()

-- | LLAMA_API int32_t llama_encode(struct llama_context * ctx, struct llama_batch batch);
foreign import ccall "llama_encode"
  c_llama_encode ::
    CLlamaContext -> -- ctx
    Ptr LlamaBatch -> -- batch
    IO CInt

-- | LLAMA_API int32_t llama_decode(struct llama_context * ctx, struct llama_batch batch);
foreign import ccall "llama_decode_wrap"
  c_llama_decode_wrap ::
    CLlamaContext -> -- ctx
    Ptr LlamaBatch -> -- batch
    IO CInt

-- | LLAMA_API void llama_set_n_threads(struct llama_context * ctx, int32_t n_threads, int32_t n_threads_batch);
foreign import ccall "llama_set_n_threads"
  c_llama_set_n_threads ::
    CLlamaContext -> -- ctx
    CInt -> -- n_threads
    CInt -> -- n_threads_batch
    IO ()

-- | LLAMA_API int32_t llama_n_threads(struct llama_context * ctx);
foreign import ccall "llama_n_threads"
  c_llama_n_threads ::
    CLlamaContext -> -- ctx
    IO CInt

-- | LLAMA_API int32_t llama_n_threads_batch(struct llama_context * ctx);
foreign import ccall "llama_n_threads_batch"
  c_llama_n_threads_batch ::
    CLlamaContext -> -- ctx
    IO CInt

-- | LLAMA_API void llama_set_embeddings(struct llama_context * ctx, CBool embeddings);
foreign import ccall "llama_set_embeddings"
  c_llama_set_embeddings ::
    CLlamaContext -> -- ctx
    CBool -> -- embeddings
    IO ()

-- | LLAMA_API void llama_set_causal_attn(struct llama_context * ctx, CBool causal_attn);
foreign import ccall "llama_set_causal_attn"
  c_llama_set_causal_attn ::
    CLlamaContext -> -- ctx
    CBool -> -- causal_attn
    IO ()

-- | LLAMA_API void llama_set_warmup(struct llama_context * ctx, CBool warmup);
foreign import ccall "llama_set_warmup"
  c_llama_set_warmup ::
    CLlamaContext -> -- ctx
    CBool -> -- warmup
    IO ()

-- | LLAMA_API void llama_set_abort_callback(struct llama_context * ctx, ggml_abort_callback abort_callback, void * abort_callback_data);
foreign import ccall "llama_set_abort_callback"
  c_llama_set_abort_callback ::
    CLlamaContext -> -- ctx
    FunPtr (Ptr () -> IO CInt) -> -- abort_callback
    Ptr () -> -- abort_callback_data
    IO ()

-- | LLAMA_API void llama_synchronize(struct llama_context * ctx);
foreign import ccall "llama_synchronize"
  c_llama_synchronize ::
    CLlamaContext -> -- ctx
    IO ()

-- | LLAMA_API float * llama_get_logits(struct llama_context * ctx);
foreign import ccall "llama_get_logits"
  c_llama_get_logits ::
    CLlamaContext -> -- ctx
    IO (Ptr CFloat)

-- | LLAMA_API float * llama_get_logits_ith(struct llama_context * ctx, int32_t i);
foreign import ccall "llama_get_logits_ith"
  c_llama_get_logits_ith ::
    CLlamaContext -> -- ctx
    CInt -> -- i
    IO (Ptr CFloat)

-- | LLAMA_API float * llama_get_embeddings(struct llama_context * ctx);
foreign import ccall "llama_get_embeddings"
  c_llama_get_embeddings ::
    CLlamaContext -> -- ctx
    IO (Ptr CFloat)

-- | LLAMA_API float * llama_get_embeddings_ith(struct llama_context * ctx, int32_t i);
foreign import ccall "llama_get_embeddings_ith"
  c_llama_get_embeddings_ith ::
    CLlamaContext -> -- ctx
    CInt -> -- i
    IO (Ptr CFloat)

-- | LLAMA_API float * llama_get_embeddings_seq(struct llama_context * ctx, llama_seq_id seq_id);
foreign import ccall "llama_get_embeddings_seq"
  c_llama_get_embeddings_seq ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id
    IO (Ptr CFloat)
