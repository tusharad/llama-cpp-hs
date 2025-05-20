{- |
Module      : Llama.Model
Description : High level Model interface for llama-cpp
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-}
module Llama.Model (
    defaultModelParams
, loadModelFromFile
, initContextFromModel
, getModelVocab
, getContextModel
, getVocabType
, getModelRoPEFreqScale
, getModelNumKVHeads
, getModelNumHeads
, getModelNumLayers
, getModelEmbeddingDim
, getModelTrainingContextSize
, getModelSize
, getModelChatTemplate
, getModelHasEncoder
, getModelNumParams
, getModelHasDecoder
, getModelDecoderStartToken
, getModelIsRecurrent
, quantizeModel
, quantizeModelDefault
, defaultQuantizeParams
, getModelMetaCount
, getModelMetaValue
, getModelMetaKeyByIndex
, getModelMetaValueByIndex
, getModelDescription
, loadModelFromSplits
, getModelRopeType
) where

import Data.Functor
import Foreign
import Foreign.C.String
import Llama.Internal.Foreign
import Llama.Internal.Types
import Llama.Internal.Types.Params

-- | Default model parameters
defaultModelParams :: IO LlamaModelParams
defaultModelParams = do
  -- Convert to a pointer to pass to the C function
  alloca $ \paramsPtr -> do
    c_llama_model_default_params (CLlamaModelParams paramsPtr)
    peek paramsPtr

-- | Load a model from a file using the specified parameters
loadModelFromFile :: FilePath -> LlamaModelParams -> IO (Either String Model)
loadModelFromFile path params = do
  withCString path $ \cPath -> do
    withStorable params $ \paramsPtr -> do
      model <- c_llama_model_load_from_file_wrap cPath (CLlamaModelParams paramsPtr)
      if model == CLlamaModel nullPtr
        then return $ Left "Failed to load model"
        else do
          let (CLlamaModel modelPtr) = model
          fp <- newForeignPtr p_llama_model_free modelPtr
          return $ Right $ Model fp

-- | Create a context from a model using the specified parameters
initContextFromModel :: Model -> LlamaContextParams -> IO (Either String Context)
initContextFromModel (Model modelFPtr) params = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    withStorable params $ \paramsPtr -> do
      context <-
        c_llama_init_from_model_wrap
          (CLlamaModel modelPtr)
          (CLlamaContextParams paramsPtr)
      if context == CLlamaContext nullPtr
        then return $ Left "Failed to initialize context"
        else do
          let (CLlamaContext contextPtr) = context
          fp <- newForeignPtr p_llama_free contextPtr
          return $ Right $ Context fp

-- | Get the vocabulary from a model
getModelVocab :: Model -> IO (Either String Vocab)
getModelVocab (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    vocab <- c_llama_model_get_vocab (CLlamaModel modelPtr)
    if vocab == CLlamaVocab nullPtr
      then return $ Left "Failed to get vocabulary"
      else do
        -- For now, assuming it's owned by the model and doesn't need separate freeing
        let (CLlamaVocab vocabPtr) = vocab
        -- Using a dummy finalizer since vocab is owned by the model
        fp <- newForeignPtr_ vocabPtr
        return $ Right $ Vocab fp

-- | Convert Storable Haskell struct to pointer and run an action
withStorable :: Storable a => a -> (Ptr a -> IO b) -> IO b
withStorable x f = alloca $ \ptr -> do
  poke ptr x
  f ptr

-- | Get the model associated with a context.
getContextModel :: Context -> IO Model
getContextModel (Context ctxFPtr) =
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    CLlamaModel modelPtr <- c_llama_get_model (CLlamaContext ctxPtr)
    fp <- newForeignPtr p_llama_model_free modelPtr
    return $ Model fp

-- | Get the vocabulary type.
getVocabType :: Vocab -> IO (Maybe LlamaVocabType)
getVocabType (Vocab vocabFPtr) =
  withForeignPtr vocabFPtr $ \vocabPtr ->
    fromLlamaRopeVocabType <$> c_llama_vocab_type (CLlamaVocab vocabPtr)

-- | Get RoPE frequency scaling factor used during training.
getModelRoPEFreqScale :: Model -> IO Float
getModelRoPEFreqScale (Model modelFPtr) =
  withForeignPtr modelFPtr $ \modelPtr ->
    realToFrac <$> c_llama_model_rope_freq_scale_train (CLlamaModel modelPtr)

-- | Get the number of key/value heads in the model.
getModelNumKVHeads :: Model -> IO Int
getModelNumKVHeads (Model modelFPtr) =
  withForeignPtr modelFPtr $ \modelPtr ->
    fromCInt <$> c_llama_model_n_head_kv (CLlamaModel modelPtr)

-- | Get the number of attention heads in the model.
getModelNumHeads :: Model -> IO Int
getModelNumHeads (Model modelFPtr) =
  withForeignPtr modelFPtr $ \modelPtr ->
    fromCInt <$> c_llama_model_n_head (CLlamaModel modelPtr)

-- | Get the number of transformer layers in the model.
getModelNumLayers :: Model -> IO Int
getModelNumLayers (Model modelFPtr) =
  withForeignPtr modelFPtr $ \modelPtr ->
    fromCInt <$> c_llama_model_n_layer (CLlamaModel modelPtr)

-- | Get the embedding dimension of the model.
getModelEmbeddingDim :: Model -> IO Int
getModelEmbeddingDim (Model modelFPtr) =
  withForeignPtr modelFPtr $ \modelPtr ->
    fromCInt <$> c_llama_model_n_embd (CLlamaModel modelPtr)

-- | Get the training context size of the model.
getModelTrainingContextSize :: Model -> IO Int
getModelTrainingContextSize (Model modelFPtr) =
  withForeignPtr modelFPtr $ \modelPtr ->
    fromCInt <$> c_llama_model_n_ctx_train (CLlamaModel modelPtr)

fromCInt :: Int32 -> Int
fromCInt = fromIntegral

-- | Get the size of the model in bytes
getModelSize :: Model -> IO Int64
getModelSize (Model modelFPtr) =
  withForeignPtr modelFPtr $ \modelPtr ->
    fromIntegral <$> c_llama_model_size (CLlamaModel modelPtr)

-- | Check if the model has an encoder
getModelHasEncoder :: Model -> IO Bool
getModelHasEncoder (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    c_llama_model_has_encoder (CLlamaModel modelPtr) <&> (/= 0)

-- | Get the chat template from a model, optionally by name
getModelChatTemplate :: Model -> Maybe String -> IO (Either String String)
getModelChatTemplate (Model modelFPtr) mName = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    cName <- maybe (pure nullPtr) newCString mName
    template <- c_llama_model_chat_template (CLlamaModel modelPtr) cName
    if template == nullPtr
      then return $ Left "Failed to get chat template"
      else Right <$> peekCString template

-- | Get the number of parameters in the model
getModelNumParams :: Model -> IO (Either String Int64)
getModelNumParams (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    params <- c_llama_model_n_params (CLlamaModel modelPtr)
    if params == 0
      then return $ Left "Failed to get number of parameters"
      else return $ Right $ fromIntegral params

-- | Check if the model has a decoder
getModelHasDecoder :: Model -> IO Bool
getModelHasDecoder (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    c_llama_model_has_decoder (CLlamaModel modelPtr) <&> (/= 0)

-- | Get the decoder start token from the model
getModelDecoderStartToken :: Model -> IO (Either String LlamaToken)
getModelDecoderStartToken (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    token_ <- c_llama_model_decoder_start_token (CLlamaModel modelPtr)
    if token_ == -1
      then return $ Left "Failed to get decoder start token"
      else return $ Right token_

-- | Check if the model is recurrent
getModelIsRecurrent :: Model -> IO Bool
getModelIsRecurrent (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    c_llama_model_is_recurrent (CLlamaModel modelPtr) <&> (/= 0)

-- | Quantize a model from a file to another file using specified parameters
quantizeModel ::
  FilePath ->
  FilePath ->
  LlamaModelQuantizeParams ->
  IO (Either String Word32)
quantizeModel inpPath outPath params = do
  withCString inpPath $ \cInpPath -> do
    withCString outPath $ \cOutPath -> do
      withStorable params $ \paramsPtr -> do
        result <-
          c_llama_model_quantize
            cInpPath
            cOutPath
            (CLlamaModelQuantizeParams paramsPtr)
        if result == 0
          then return $ Left "Failed to quantize model"
          else return $ Right result

-- | Quantize a model from a file to another file using default parameters
quantizeModelDefault :: FilePath -> FilePath -> IO (Either String Word32)
quantizeModelDefault inpPath outPath = do
  params <- defaultQuantizeParams
  quantizeModel inpPath outPath params

-- | Get the default quantization parameters
defaultQuantizeParams :: IO LlamaModelQuantizeParams
defaultQuantizeParams = do
  (CLlamaModelQuantizeParams paramsPtr) <- c_llama_model_quantize_default_params
  peek paramsPtr

-- | Get a metadata value as a string from a model
getModelMetaValue :: Model -> String -> IO (Either String String)
getModelMetaValue (Model modelFPtr) key = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    withCString key $ \cKey -> do
      allocaArray 256 $ \bufPtr -> do
        result <- c_llama_model_meta_val_str (CLlamaModel modelPtr) cKey bufPtr 256
        if result == -1
          then return $ Left "Failed to get metadata value"
          else do
            str <- peekCString bufPtr
            return $ Right str

-- | Get the number of metadata entries in a model
getModelMetaCount :: Model -> IO Int
getModelMetaCount (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    fromIntegral <$> c_llama_model_meta_count (CLlamaModel modelPtr)

-- | Get a metadata key by index from a model
getModelMetaKeyByIndex :: Model -> Int -> IO (Either String String)
getModelMetaKeyByIndex (Model modelFPtr) index = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    allocaArray 256 $ \bufPtr -> do
      result <-
        c_llama_model_meta_key_by_index
          (CLlamaModel modelPtr)
          (fromIntegral index)
          bufPtr
          256
      if result == -1
        then return $ Left "Failed to get metadata key"
        else do
          str <- peekCString bufPtr
          return $ Right str

-- | Get a metadata value by index from a model
getModelMetaValueByIndex :: Model -> Int -> IO (Either String String)
getModelMetaValueByIndex (Model modelFPtr) index = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    allocaArray 256 $ \bufPtr -> do
      result <-
        c_llama_model_meta_val_str_by_index
          (CLlamaModel modelPtr)
          (fromIntegral index)
          bufPtr
          256
      if result == -1
        then return $ Left "Failed to get metadata value"
        else do
          str <- peekCString bufPtr
          return $ Right str

-- | Get a model description
getModelDescription :: Model -> IO String
getModelDescription (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    allocaArray 256 $ \bufPtr -> do
      _ <- c_llama_model_desc (CLlamaModel modelPtr) bufPtr 256
      peekCString bufPtr

-- | Load a model from multiple file paths using specified parameters
loadModelFromSplits :: [FilePath] -> LlamaModelParams -> IO (Either String Model)
loadModelFromSplits paths params = do
  withStorable params $ \paramsPtr -> do
    pathsPtr <- newArrayOfPtrs paths
    withForeignPtr pathsPtr $ \pathsPtr' -> do
      model <-
        c_llama_model_load_from_splits
          pathsPtr'
          (fromIntegral (length paths))
          (CLlamaModelParams paramsPtr)
      if model == CLlamaModel nullPtr
        then return $ Left "Failed to load model"
        else do
          let (CLlamaModel modelPtr) = model
          fp <- newForeignPtr p_llama_model_free modelPtr
          return $ Right $ Model fp

-- | Get the RoPE type from a model
getModelRopeType :: Model -> IO (Maybe LlamaRopeTypeScaling)
getModelRopeType (Model modelFPtr) = do
  withForeignPtr modelFPtr $ \modelPtr -> do
    alloca $ \outPtr -> do
      c_llama_model_rope_type_into (CLlamaModel modelPtr) outPtr
      val <- peek outPtr
      return $ fromLlamaRopeTypeScaling val

newArrayOfPtrs :: [FilePath] -> IO (ForeignPtr CString)
newArrayOfPtrs xs = do
  ptrs <- mallocBytes (length xs * sizeOf (undefined :: Ptr CString))
  mapM_
    ( \(i, x) -> withCString x $ \cstr ->
        poke
          (castPtr ptrs `plusPtr` (i * sizeOf (undefined :: Ptr CString)))
          cstr
    )
    (zip [0 ..] xs)
  newForeignPtr_ ptrs
