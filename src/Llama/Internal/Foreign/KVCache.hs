module Llama.Internal.Foreign.KVCache (
 
  c_llama_kv_cache_view_init_into
  , c_llama_kv_cache_view_free
  , c_llama_kv_cache_view_update
  , c_llama_kv_self_n_tokens
  , c_llama_kv_self_used_cells
  , c_llama_kv_self_clear
  , c_llama_kv_self_seq_rm
  , c_llama_kv_self_seq_cp
  , c_llama_kv_self_seq_keep
  , c_llama_kv_self_seq_add
  , c_llama_kv_self_seq_div
  , c_llama_kv_self_seq_pos_max
  , c_llama_kv_self_defrag
  , c_llama_kv_self_can_shift
  , c_llama_kv_self_update

) where

import Foreign
import Foreign.C
import Llama.Internal.Types

-- | LLAMA_API struct llama_kv_cache_view llama_kv_cache_view_init(const struct llama_context * ctx, int32_t n_seq_max);
foreign import ccall "llama_kv_cache_view_init_into"
    c_llama_kv_cache_view_init_into ::
        CLlamaContext ->  -- ctx
        CInt ->              -- n_seq_max
        Ptr LlamaKvCacheView ->
        IO ()

-- | LLAMA_API void llama_kv_cache_view_free(struct llama_kv_cache_view * view);
foreign import ccall "llama_kv_cache_view_free"
    c_llama_kv_cache_view_free ::
        Ptr LlamaKvCacheView ->  -- view
        IO ()

-- |LLAMA_API void llama_kv_cache_view_update(const struct llama_context * ctx, struct llama_kv_cache_view * view);
foreign import ccall "llama_kv_cache_view_update"
    c_llama_kv_cache_view_update ::
        CLlamaContext ->      -- ctx
        Ptr LlamaKvCacheView ->   -- view
        IO ()

-- | LLAMA_API int32_t llama_kv_self_n_tokens(const struct llama_context * ctx);
foreign import ccall "llama_kv_self_n_tokens"
  c_llama_kv_self_n_tokens ::
    CLlamaContext -> -- ctx
    IO CInt

-- | LLAMA_API int32_t llama_kv_self_used_cells(const struct llama_context * ctx);
foreign import ccall "llama_kv_self_used_cells"
  c_llama_kv_self_used_cells ::
    CLlamaContext -> -- ctx
    IO CInt

-- | LLAMA_API void llama_kv_self_clear(struct llama_context * ctx);
foreign import ccall "llama_kv_self_clear"
  c_llama_kv_self_clear ::
    CLlamaContext -> -- ctx
    IO ()

{- | LLAMA_API bool llama_kv_self_seq_rm(
|         struct llama_context * ctx, llama_seq_id seq_id, llama_pos p0, llama_pos p1);
-}
foreign import ccall "llama_kv_self_seq_rm"
  c_llama_kv_self_seq_rm ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id
    LlamaPos -> -- p0
    LlamaPos -> -- p1
    IO CBool

{- | LLAMA_API void llama_kv_self_seq_cp(
|         struct llama_context * ctx, llama_seq_id seq_id_src, llama_seq_id seq_id_dst, llama_pos p0, llama_pos p1);
-}
foreign import ccall "llama_kv_self_seq_cp"
  c_llama_kv_self_seq_cp ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id_src
    LlamaSeqId -> -- seq_id_dst
    LlamaPos -> -- p0
    LlamaPos -> -- p1
    IO ()

-- | LLAMA_API void llama_kv_self_seq_keep(struct llama_context * ctx, llama_seq_id seq_id);
foreign import ccall "llama_kv_self_seq_keep"
  c_llama_kv_self_seq_keep ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id
    IO ()

{- | LLAMA_API void llama_kv_self_seq_add(
|         struct llama_context * ctx, llama_seq_id seq_id, llama_pos p0, llama_pos p1, llama_pos delta);
-}
foreign import ccall "llama_kv_self_seq_add"
  c_llama_kv_self_seq_add ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id
    LlamaPos -> -- p0
    LlamaPos -> -- p1
    LlamaPos -> -- delta
    IO ()

{- | LLAMA_API void llama_kv_self_seq_div(
|         struct llama_context * ctx, llama_seq_id seq_id, llama_pos p0, llama_pos p1, int d);
-}
foreign import ccall "llama_kv_self_seq_div"
  c_llama_kv_self_seq_div ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id
    LlamaPos -> -- p0
    LlamaPos -> -- p1
    CInt -> -- d
    IO ()

-- | LLAMA_API llama_pos llama_kv_self_seq_pos_max(struct llama_context * ctx, llama_seq_id seq_id);
foreign import ccall "llama_kv_self_seq_pos_max"
  c_llama_kv_self_seq_pos_max ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id
    IO LlamaPos

-- | LLAMA_API void llama_kv_self_defrag(struct llama_context * ctx);
foreign import ccall "llama_kv_self_defrag"
  c_llama_kv_self_defrag ::
    CLlamaContext -> -- ctx
    IO ()

-- | LLAMA_API bool llama_kv_self_can_shift(const struct llama_context * ctx);
foreign import ccall "llama_kv_self_can_shift"
  c_llama_kv_self_can_shift ::
    Ptr CLlamaContext -> -- ctx
    IO CBool

-- | LLAMA_API void llama_kv_self_update(struct llama_context * ctx);
foreign import ccall "llama_kv_self_update"
  c_llama_kv_self_update ::
    CLlamaContext -> -- ctx
    IO ()
