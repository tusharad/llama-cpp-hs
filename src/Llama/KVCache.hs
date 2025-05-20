{- |
Module      : Llama.KVCache
Description : High level KVCache interface for llama-cpp
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-}
module Llama.KVCache (
    kvCacheViewInit
, kvSelfSeqAdd
, kvSelfSeqDiv
, kvSelfSeqPosMax
, kvSelfDefrag
, kvSelfCanShift
, kvSelfUpdate
, kvSelfSeqKeep
, kvSelfSeqCopy
, kvSelfSeqRemove
, kvSelfClear
, kvSelfUsedCells
, kvSelfNumTokens
, kvCacheViewUpdate
) where

import Llama.Internal.Types
import Foreign
import Llama.Internal.Foreign

{-
TODO: no free function for struct llama_kv_cache *
No one is using struct llama_kv_cache
-- | Get the KV cache associated with this context.
getKVCache :: Context -> IO KVCache
getKVCache (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    ptr <- c_llama_get_kv_self (CLlamaContext ctxPtr)
    -- We assume finalization is handled elsewhere or use a no-op finalizer
    fp <- newForeignPtr_ ptr -- assumes Ptr () is actually Ptr CLlamaKVCache
    return $ KVCache fp
-}

-- | Convenience wrapper that allocates a LlamaKvCacheView and initializes it
kvCacheViewInit :: Context -> Int -> IO LlamaKvCacheView
kvCacheViewInit (Context fPtr) n_seq_max_ = do
    withForeignPtr fPtr $ \contextPtr -> do
      alloca $ \pView -> do
        c_llama_kv_cache_view_init_into (CLlamaContext contextPtr) (fromIntegral n_seq_max_) pView
        peek pView

-- | Shift positions in a sequence by a delta.
kvSelfSeqAdd ::
  Context ->
  LlamaSeqId -> -- seq_id
  LlamaPos -> -- p0
  LlamaPos -> -- p1
  LlamaPos -> -- delta
  IO ()
kvSelfSeqAdd (Context ctxFPtr) seqId p0 p1 delta =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_seq_add (CLlamaContext ctxPtr) seqId p0 p1 delta

-- | Divide positions in a sequence by a factor (used for attention windowing).
kvSelfSeqDiv ::
  Context ->
  LlamaSeqId -> -- seq_id
  LlamaPos -> -- p0
  LlamaPos -> -- p1
  Int -> -- d
  IO ()
kvSelfSeqDiv (Context ctxFPtr) seqId p0 p1 d =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_seq_div (CLlamaContext ctxPtr) seqId p0 p1 (fromIntegral d)

-- | Get the maximum position stored in the KV cache for a given sequence.
kvSelfSeqPosMax ::
  Context ->
  LlamaSeqId -> -- seq_id
  IO LlamaPos
kvSelfSeqPosMax (Context ctxFPtr) seqId =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_seq_pos_max (CLlamaContext ctxPtr) seqId

-- | Defragment the KV cache to optimize memory usage.
kvSelfDefrag ::
  Context ->
  IO ()
kvSelfDefrag (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_defrag (CLlamaContext ctxPtr)

-- | Check whether the KV cache can be shifted (e.g., due to full buffer).
kvSelfCanShift ::
  Context ->
  IO Bool
kvSelfCanShift (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    toBool <$> c_llama_kv_self_can_shift (castPtr ctxPtr)

-- | Update the KV cache's internal state (e.g., after manual modifications).
kvSelfUpdate ::
  Context ->
  IO ()
kvSelfUpdate (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_update (CLlamaContext ctxPtr)

-- | Prevent a sequence from being removed during KV cache cleanup.
kvSelfSeqKeep ::
  Context ->
  LlamaSeqId -> -- seq_id
  IO ()
kvSelfSeqKeep (Context ctxFPtr) seqId =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_seq_keep (CLlamaContext ctxPtr) seqId

-- | Copy a range of positions from one sequence to another.
kvSelfSeqCopy ::
  Context ->
  LlamaSeqId -> -- seq_id_src
  LlamaSeqId -> -- seq_id_dst
  LlamaPos -> -- p0
  LlamaPos -> -- p1
  IO ()
kvSelfSeqCopy (Context ctxFPtr) srcId dstId p0 p1 =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_seq_cp (CLlamaContext ctxPtr) srcId dstId p0 p1

-- | Remove a range of positions from a sequence.
kvSelfSeqRemove ::
  Context ->
  LlamaSeqId -> -- seq_id
  LlamaPos -> -- p0
  LlamaPos -> -- p1
  IO Bool
kvSelfSeqRemove (Context ctxFPtr) seqId p0 p1 =
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    result <- c_llama_kv_self_seq_rm (CLlamaContext ctxPtr) seqId p0 p1
    return $ toBool result

-- | Clear all sequences from the KV cache.
kvSelfClear ::
  Context ->
  IO ()
kvSelfClear (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_self_clear (CLlamaContext ctxPtr)

-- | Get number of used cells in the KV cache.
kvSelfUsedCells ::
  Context ->
  IO Int
kvSelfUsedCells (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> c_llama_kv_self_used_cells (CLlamaContext ctxPtr)

-- | Get total number of tokens currently stored in the KV cache.
kvSelfNumTokens ::
  Context ->
  IO Int
kvSelfNumTokens (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> c_llama_kv_self_n_tokens (CLlamaContext ctxPtr)

-- | Update the KV cache view to reflect current state.
kvCacheViewUpdate ::
  Context ->
  Ptr LlamaKvCacheView -> -- view
  IO ()
kvCacheViewUpdate (Context ctxFPtr) view =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_kv_cache_view_update (CLlamaContext ctxPtr) view
