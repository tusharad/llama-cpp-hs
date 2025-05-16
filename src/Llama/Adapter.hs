module Llama.Adapter where

import Foreign
import Foreign.C.String
import Llama.Internal.Foreign
import Llama.Internal.Types
import Foreign.C (CFloat)

{- | Initialize a LoRA adapter from a file path.
This function wraps the C function 'llama_adapter_lora_init'
and returns a managed AdapterLora object.
-}
initAdapterLora :: Model -> FilePath -> IO (Either String AdapterLora)
initAdapterLora (Model modelFPtr) path = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    withCString path $ \cPath -> do
      adapter@(CLlamaAdapterLora adapterPtr) <-
        c_llama_adapter_lora_init (CLlamaModel modelPtr) cPath
      if adapter == CLlamaAdapterLora nullPtr
        then return $ Left "Failed to initialize LoRA adapter"
        else do
          -- We directly use c_llama_adapter_lora_free as the finalizer
          fp <- newForeignPtr p_llama_adapter_lora_free adapterPtr
          return $ Right $ AdapterLora fp

-- | Apply a LoRA adapter with a given scale.
setAdapterLora :: Context -> AdapterLora -> Float -> IO (Either String ())
setAdapterLora (Context ctxFPtr) (AdapterLora adapterFPtr) scale = do
  result <- withForeignPtr ctxFPtr $ \ctxPtr ->
    withForeignPtr adapterFPtr $ \adapterPtr ->
      c_llama_set_adapter_lora
        (CLlamaContext ctxPtr)
        (CLlamaAdapterLora adapterPtr)
        (realToFrac scale)
  if result == -1
    then return $ Left "Failed to set LoRA adapter"
    else return $ Right ()

-- | Remove a previously applied LoRA adapter.
rmAdapterLora :: Context -> AdapterLora -> IO (Either String ())
rmAdapterLora (Context ctxFPtr) (AdapterLora adapterFPtr) = do
  result <- withForeignPtr ctxFPtr $ \ctxPtr ->
    withForeignPtr adapterFPtr $ \adapterPtr ->
      c_llama_rm_adapter_lora (CLlamaContext ctxPtr) (CLlamaAdapterLora adapterPtr)
  if result == -1
    then return $ Left "Failed to remove LoRA adapter"
    else return $ Right ()

-- | Clear all active LoRA adapters from the context.
clearAdapterLora :: Context -> IO ()
clearAdapterLora (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_clear_adapter_lora (CLlamaContext ctxPtr)

-- | Apply a context vector (cvec).
applyAdapterCVec :: Context -> Maybe [Float] -> Int -> Int -> Int -> IO (Either String ())
applyAdapterCVec (Context ctxFPtr) mValues n_embd il_start il_end = do
  (ptr, len) <- case mValues of
    Nothing -> pure (nullPtr, 0 :: Int)
    Just vs -> do
      arrayPtr <- floatArrayToPtr vs
      pure (arrayPtr, length vs)
  result <- withForeignPtr ctxFPtr $ \ctxPtr ->
    c_llama_apply_adapter_cvec
      (CLlamaContext ctxPtr)
      ptr
      (fromIntegral len)
      (fromIntegral n_embd)
      (fromIntegral il_start)
      (fromIntegral il_end)
  if result == -1
    then return $ Left "Failed to apply context vector"
    else return $ Right ()

floatArrayToPtr :: [Float] -> IO (Ptr CFloat)
floatArrayToPtr xs = do
  ptr <- mallocBytes (length xs * sizeOf (undefined :: CFloat))
  pokeArray ptr (map realToFrac xs)
  return ptr
