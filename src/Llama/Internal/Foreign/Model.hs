module Llama.Internal.Foreign.Model (
 
  p_llama_free
  , p_llama_model_free
  , c_llama_free
  , c_llama_init_from_model_wrap
  , c_llama_model_default_params
  , c_llama_model_load_from_file_wrap
  , c_llama_model_free
  , c_llama_model_get_vocab
  , c_llama_model_load_from_splits
  , c_llama_model_rope_type_into
  , c_llama_model_n_ctx_train
  , c_llama_model_n_embd
  , c_llama_model_n_layer
  , c_llama_model_n_head
  , c_llama_model_n_head_kv
  , c_llama_get_model
  , c_llama_model_rope_freq_scale_train
  , c_llama_vocab_type
  , c_llama_model_meta_val_str
  , c_llama_model_meta_count
  , c_llama_model_meta_key_by_index
  , c_llama_model_meta_val_str_by_index
  , c_llama_model_desc
  , c_llama_model_size
  , c_llama_model_chat_template
  , c_llama_model_n_params
  , c_llama_model_has_encoder
  , c_llama_model_has_decoder
  , c_llama_model_decoder_start_token
  , c_llama_model_is_recurrent
  , c_llama_model_quantize
  , c_llama_model_quantize_default_params

) where

import Foreign
import Foreign.C
import Llama.Internal.Types
import Llama.Internal.Types.Params

-- | Foreign pointer to the C function for freeing the context.
foreign import ccall "&llama_free"
  p_llama_free :: FinalizerPtr CLlamaContext

-- | Foreign pointer to the C function for freeing the model.
foreign import ccall "&llama_model_free"
  p_llama_model_free :: FinalizerPtr CLlamaModel

foreign import ccall "llama_free" c_llama_free :: CLlamaContext -> IO ()

foreign import ccall "llama_init_from_model_into"
  c_llama_init_from_model_wrap ::
    CLlamaModel -> CLlamaContextParams -> IO CLlamaContext

foreign import ccall "llama_model_default_params"
  c_llama_model_default_params :: CLlamaModelParams -> IO ()

foreign import ccall "llama_model_load_from_file_wrap"
  c_llama_model_load_from_file_wrap ::
    CString -> CLlamaModelParams -> IO CLlamaModel

foreign import ccall "llama_model_free"
  c_llama_model_free :: CLlamaModel -> IO ()

foreign import ccall "llama_model_get_vocab"
  c_llama_model_get_vocab :: CLlamaModel -> IO CLlamaVocab

-- LLAMA_API struct llama_model * llama_model_load_from_splits(
--                              const char ** paths,
--                                  size_t    n_paths,
--               struct llama_model_params    params);
foreign import ccall "llama_model_load_from_splits"
  c_llama_model_load_from_splits ::
    Ptr CString -> CSize -> CLlamaModelParams -> IO CLlamaModel

-- | Get RoPE type from model
foreign import ccall safe "llama_model_rope_type_into"
  c_llama_model_rope_type_into :: CLlamaModel -> Ptr CInt -> IO ()

-- | Get training context size
foreign import ccall "llama_model_n_ctx_train"
  c_llama_model_n_ctx_train :: CLlamaModel -> IO Int32

-- | Get embedding dimension
foreign import ccall "llama_model_n_embd"
  c_llama_model_n_embd :: CLlamaModel -> IO Int32

-- | Get number of layers
foreign import ccall "llama_model_n_layer"
  c_llama_model_n_layer :: CLlamaModel -> IO Int32

-- | Get number of heads
foreign import ccall "llama_model_n_head"
  c_llama_model_n_head :: CLlamaModel -> IO Int32

-- | Get number of key/value heads
foreign import ccall "llama_model_n_head_kv"
  c_llama_model_n_head_kv :: CLlamaModel -> IO Int32

-- | Get model from context
foreign import ccall "llama_get_model"
  c_llama_get_model :: CLlamaContext -> IO CLlamaModel

-- For float llama_model_rope_freq_scale_train(const struct llama_model * model);
foreign import ccall "llama_model_rope_freq_scale_train"
  c_llama_model_rope_freq_scale_train :: CLlamaModel -> IO CFloat

-- For enum llama_vocab_type llama_vocab_type(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_type"
  c_llama_vocab_type :: CLlamaVocab -> IO CInt

-- | LLAMA_API int32_t llama_model_meta_val_str(const struct llama_model * model, const char * key, char * buf, size_t buf_size);
foreign import ccall "llama_model_meta_val_str"
  c_llama_model_meta_val_str ::
    CLlamaModel -> -- model
    CString -> -- key
    CString -> -- buffer
    CSize -> -- buffer size
    IO CInt

-- | LLAMA_API int32_t llama_model_meta_count(const struct llama_model * model);
foreign import ccall "llama_model_meta_count"
  c_llama_model_meta_count ::
    CLlamaModel -> -- model
    IO CInt

-- | LLAMA_API int32_t llama_model_meta_key_by_index(const struct llama_model * model, int32_t i, char * buf, size_t buf_size);
foreign import ccall "llama_model_meta_key_by_index"
  c_llama_model_meta_key_by_index ::
    CLlamaModel -> -- model
    CInt -> -- index
    CString -> -- buffer
    CSize -> -- buffer size
    IO CInt

-- | LLAMA_API int32_t llama_model_meta_val_str_by_index(const struct llama_model * model, int32_t i, char * buf, size_t buf_size);
foreign import ccall "llama_model_meta_val_str_by_index"
  c_llama_model_meta_val_str_by_index ::
    CLlamaModel -> -- model
    CInt -> -- index
    CString -> -- buffer
    CSize -> -- buffer size
    IO CInt

-- | LLAMA_API int32_t llama_model_desc(const struct llama_model * model, char * buf, size_t buf_size);
foreign import ccall "llama_model_desc"
  c_llama_model_desc ::
    CLlamaModel -> -- model
    CString -> -- buffer
    CSize -> -- buffer size
    IO CInt

-- | LLAMA_API uint64_t llama_model_size(const struct llama_model * model);
foreign import ccall "llama_model_size"
  c_llama_model_size ::
    CLlamaModel -> -- model
    IO Word64

-- | LLAMA_API const char * llama_model_chat_template(const struct llama_model * model, const char * name);
foreign import ccall "llama_model_chat_template"
  c_llama_model_chat_template ::
    CLlamaModel -> -- model
    CString -> -- optional name (NULL = default)
    IO CString

-- | LLAMA_API uint64_t llama_model_n_params(const struct llama_model * model);
foreign import ccall "llama_model_n_params"
  c_llama_model_n_params ::
    CLlamaModel -> -- model
    IO Word64

-- | LLAMA_API bool llama_model_has_encoder(const struct llama_model * model);
foreign import ccall "llama_model_has_encoder"
  c_llama_model_has_encoder ::
    CLlamaModel -> -- model
    IO CBool

-- | LLAMA_API bool llama_model_has_decoder(const struct llama_model * model);
foreign import ccall "llama_model_has_decoder"
  c_llama_model_has_decoder ::
    CLlamaModel -> -- model
    IO CBool

-- | LLAMA_API llama_token llama_model_decoder_start_token(const struct llama_model * model);
foreign import ccall "llama_model_decoder_start_token"
  c_llama_model_decoder_start_token ::
    CLlamaModel -> -- model
    IO LlamaToken

-- | LLAMA_API bool llama_model_is_recurrent(const struct llama_model * model);
foreign import ccall "llama_model_is_recurrent"
  c_llama_model_is_recurrent ::
    CLlamaModel -> -- model
    IO CBool

{- | LLAMA_API uint32_t llama_model_quantize(
            const char * fname_inp,
            const char * fname_out,
            const llama_model_quantize_params * params);
-}
foreign import ccall "llama_model_quantize"
  c_llama_model_quantize ::
    CString -> -- fname_inp
    CString -> -- fname_out
    CLlamaModelQuantizeParams -> -- params
    IO Word32

-- LLAMA_API struct llama_model_quantize_params llama_model_quantize_default_params(void);
foreign import ccall "llama_model_quantize_default_params"
  c_llama_model_quantize_default_params :: IO CLlamaModelQuantizeParams
