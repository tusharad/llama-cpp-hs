{- |
Module      : Llama.Decode
Description : High level Decode interface for llama-cpp
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-}
module Llama.Decode
  ( batchInit
  , batchGetOne
  , freeBatch
  , encodeBatch
  , decodeBatch
  , setThreadCount
  , getThreadCount
  , getBatchThreadCount
  , setEmbeddingsEnabled
  , areEmbeddingsEnabled
  , setCausalAttention
  , setThreadCounts
  , setWarmupMode
  , synchronizeContext
  ) where

import Foreign
import Llama.Internal.Foreign
import Llama.Internal.Types

batchInit :: Int -> Int -> Int -> IO Batch
batchInit nTokens embd_ nSeqMax = do
  let cTokens = fromIntegral nTokens
      cEmb = fromIntegral embd_
      cSeqMax = fromIntegral nSeqMax
  alloca $ \ptr -> do
    c_llama_batch_init_into cTokens cEmb cSeqMax ptr
    return $ Batch ptr

-- | Create a batch from a list of tokens.
batchGetOne :: [LlamaToken] -> IO Batch
batchGetOne tokens = do
  let nTokens = length tokens
  alloca $ \ptr -> do
    withArray tokens $ \tokensPtr -> do
      c_llama_batch_get_one_into tokensPtr (fromIntegral nTokens) ptr
      return $ Batch ptr

-- | Free a batch of tokens allocated with initBatch
freeBatch :: Ptr LlamaBatch -> IO ()
freeBatch = c_llama_batch_free_wrap

-- | Encode tokens using the model context.
encodeBatch :: Context -> Batch -> IO (Either String ())
encodeBatch (Context ctxFPtr) (Batch batchPtr) = do
  result <- withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_encode (CLlamaContext ctxPtr) batchPtr
  if result < 0
    then return $ Left "Encoding failed"
    else return $ Right ()

-- | Decode tokens using the model context.
decodeBatch :: Context -> Batch -> IO (Either String ())
decodeBatch (Context ctxFPtr) (Batch batchPtr) = do
  result <- withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_decode_wrap (CLlamaContext ctxPtr) batchPtr
  if result < 0
    then return $ Left "Decoding failed"
    else return $ Right ()

-- | Set number of threads used for processing.
setThreadCount :: Context -> Int -> IO ()
setThreadCount ctx_ n = setThreadCounts ctx_ n n

-- | Set main and batch thread counts separately.
setThreadCounts :: Context -> Int -> Int -> IO ()
setThreadCounts (Context ctxFPtr) nThreads nBatchThreads =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_set_n_threads
      (CLlamaContext ctxPtr)
      (fromIntegral nThreads)
      (fromIntegral nBatchThreads)

-- | Get current main thread count.
getThreadCount :: Context -> IO Int
getThreadCount (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> c_llama_n_threads (CLlamaContext ctxPtr)

-- | Get current batch thread count.
getBatchThreadCount :: Context -> IO Int
getBatchThreadCount (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> c_llama_n_threads_batch (CLlamaContext ctxPtr)

-- | Enable or disable embeddings output.
setEmbeddingsEnabled :: Context -> Bool -> IO ()
setEmbeddingsEnabled (Context ctxFPtr) enabled =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_set_embeddings (CLlamaContext ctxPtr) (fromBool enabled)

-- | Check if embeddings are enabled.
areEmbeddingsEnabled :: Context -> IO Bool
areEmbeddingsEnabled (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    resPtr <- c_llama_get_embeddings (CLlamaContext ctxPtr)
    toBool <$> peek resPtr

-- | Set causal attention mode.
setCausalAttention :: Context -> Bool -> IO ()
setCausalAttention (Context ctxFPtr) causal =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_set_causal_attn (CLlamaContext ctxPtr) (fromBool causal)

-- | Set warmup mode (e.g. precompute KV cache).
setWarmupMode :: Context -> Bool -> IO ()
setWarmupMode (Context ctxFPtr) warm =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_set_warmup (CLlamaContext ctxPtr) (fromBool warm)

{-
type AbortCallback = Ptr () -> IO CInt
-- | Set an abort callback to cancel long-running ops from another thread.
setAbortCallback :: Context -> FunPtr AbortCallback -> Ptr () -> IO ()
setAbortCallback (Context ctxFPtr) callback cbData =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_set_abort_callback (CLlamaContext ctxPtr) callback cbData
    -}

-- | Block until all async work is complete.
synchronizeContext :: Context -> IO ()
synchronizeContext (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_synchronize (CLlamaContext ctxPtr)
