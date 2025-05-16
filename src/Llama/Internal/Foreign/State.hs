module Llama.Internal.Foreign.State where

import Foreign
import Foreign.C
import Llama.Internal.Types

-- | LLAMA_API size_t llama_state_get_size(struct llama_context * ctx);
foreign import ccall "llama_state_get_size"
  c_llama_state_get_size ::
    CLlamaContext -> -- ctx
    IO CSize

-- | LLAMA_API size_t llama_state_get_data(struct llama_context * ctx, uint8_t * dst, size_t size);
foreign import ccall "llama_state_get_data"
  c_llama_state_get_data ::
    CLlamaContext -> -- ctx
    Ptr Word8 -> -- dst (buffer)
    CSize -> -- size
    IO CSize

-- | LLAMA_API size_t llama_state_set_data(struct llama_context * ctx, const uint8_t * src, size_t size);
foreign import ccall "llama_state_set_data"
  c_llama_state_set_data ::
    CLlamaContext -> -- ctx
    Ptr Word8 -> -- src (buffer)
    CSize -> -- size
    IO CSize

-- | LLAMA_API bool llama_state_load_file(struct llama_context * ctx, const char * path_session, llama_token * tokens_out, size_t n_token_capacity, size_t * n_token_count_out);
foreign import ccall "llama_state_load_file"
  c_llama_state_load_file ::
    CLlamaContext -> -- ctx
    CString -> -- path_session
    Ptr LlamaToken -> -- tokens_out
    CSize -> -- n_token_capacity
    Ptr CSize -> -- n_token_count_out
    IO CBool

-- | LLAMA_API bool llama_state_save_file(struct llama_context * ctx, const char * path_session, const llama_token * tokens, size_t n_token_count);
foreign import ccall "llama_state_save_file"
  c_llama_state_save_file ::
    CLlamaContext -> -- ctx
    CString -> -- path_session
    Ptr LlamaToken -> -- tokens
    CSize -> -- n_token_count
    IO CBool

-- | LLAMA_API size_t llama_state_seq_get_size(struct llama_context * ctx, llama_seq_id seq_id);
foreign import ccall "llama_state_seq_get_size"
  c_llama_state_seq_get_size ::
    CLlamaContext -> -- ctx
    LlamaSeqId -> -- seq_id
    IO CSize

-- | LLAMA_API size_t llama_state_seq_get_data(struct llama_context * ctx, uint8_t * dst, size_t size, llama_seq_id seq_id);
foreign import ccall "llama_state_seq_get_data"
  c_llama_state_seq_get_data ::
    CLlamaContext -> -- ctx
    Ptr Word8 -> -- dst (buffer)
    CSize -> -- size
    LlamaSeqId -> -- seq_id
    IO CSize

-- | LLAMA_API size_t llama_state_seq_set_data(struct llama_context * ctx, const uint8_t * src, size_t size, llama_seq_id dest_seq_id);
foreign import ccall "llama_state_seq_set_data"
  c_llama_state_seq_set_data ::
    CLlamaContext -> -- ctx
    Ptr Word8 -> -- src (buffer)
    CSize -> -- size
    LlamaSeqId -> -- dest_seq_id
    IO CSize

-- | LLAMA_API size_t llama_state_seq_save_file(struct llama_context * ctx, const char * filepath, llama_seq_id seq_id, const llama_token * tokens, size_t n_token_count);
foreign import ccall "llama_state_seq_save_file"
  c_llama_state_seq_save_file ::
    CLlamaContext -> -- ctx
    CString -> -- filepath
    LlamaSeqId -> -- seq_id
    Ptr LlamaToken -> -- tokens
    CSize -> -- n_token_count
    IO CSize

-- | LLAMA_API size_t llama_state_seq_load_file(struct llama_context * ctx, const char * filepath, llama_seq_id dest_seq_id, llama_token * tokens_out, size_t n_token_capacity, size_t * n_token_count_out);
foreign import ccall "llama_state_seq_load_file"
  c_llama_state_seq_load_file ::
    CLlamaContext -> -- ctx
    CString -> -- filepath
    LlamaSeqId -> -- dest_seq_id
    Ptr LlamaToken -> -- tokens_out
    CSize -> -- n_token_capacity
    Ptr CSize -> -- n_token_count_out
    IO CSize
