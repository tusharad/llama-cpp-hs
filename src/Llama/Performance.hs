module Llama.Performance where

import Llama.Internal.Types
import Foreign
import Llama.Internal.Foreign

-- | Print performance information for a context
printContextPerformance :: Context -> IO ()
printContextPerformance (Context ctxFPtr) = 
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_perf_context_print (CLlamaContext ctxPtr)

-- | Reset performance information for a context
resetContextPerformance :: Context -> IO ()
resetContextPerformance (Context ctxFPtr) = 
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_perf_context_reset (CLlamaContext ctxPtr)

-- | Print performance information for a sampler chain
printSamplerPerformance :: Sampler -> IO ()
printSamplerPerformance (Sampler samplerFPtr) = 
  withForeignPtr samplerFPtr $ \samplerPtr ->
    c_llama_perf_sampler_print samplerPtr

-- | Reset performance information for a sampler chain
resetSamplerPerformance :: Sampler -> IO ()
resetSamplerPerformance (Sampler samplerFPtr) = 
  withForeignPtr samplerFPtr $ \samplerPtr ->
    c_llama_perf_sampler_reset samplerPtr

-- | Get performance data for a context
getContextPerformance :: Context -> IO LlamaPerfContextData
getContextPerformance (Context ctxFPtr) = do
  alloca $ \perfDataPtr -> do
    withForeignPtr ctxFPtr $ \ctxPtr -> do
      c_llama_perf_context (CLlamaContext ctxPtr) perfDataPtr
    peek perfDataPtr

-- | Get performance data for a sampler chain
getSamplerPerformance :: Sampler -> IO LlamaPerfSamplerData
getSamplerPerformance (Sampler samplerFPtr) = do
  alloca $ \perfDataPtr -> do
    withForeignPtr samplerFPtr $ \samplerPtr -> do
      c_llama_perf_sampler samplerPtr perfDataPtr
    peek perfDataPtr