module Llama.Internal.Foreign.Split (
  c_llama_split_path
  , c_llama_split_prefix
  , c_llama_print_system_info
  , c_llama_log_set
) where

import Foreign
import Foreign.C

-- | LLAMA_API int llama_split_path(char * split_path, size_t maxlen, const char * path_prefix, int split_no, int split_count);
foreign import ccall "llama_split_path"
  c_llama_split_path ::
    CString -> -- split_path (output buffer)
    CSize -> -- maxlen
    CString -> -- path_prefix
    CInt -> -- split_no
    CInt -> -- split_count

    -- | Returns length of written string or required buffer size
    IO CInt

-- | LLAMA_API int llama_split_prefix(char * split_prefix, size_t maxlen, const char * split_path, int split_no, int split_count);
foreign import ccall "llama_split_prefix"
  c_llama_split_prefix ::
    CString -> -- split_prefix (output buffer)
    CSize -> -- maxlen
    CString -> -- split_path
    CInt -> -- split_no
    CInt -> -- split_count

    -- | Returns length of written string or required buffer size
    IO CInt

-- | LLAMA_API const char * llama_print_system_info(void);
foreign import ccall "llama_print_system_info"
  c_llama_print_system_info ::
    -- | Returns system info string
    IO CString

-- | LLAMA_API void llama_log_set(ggml_log_callback log_callback, void * user_data);
foreign import ccall "llama_log_set"
  c_llama_log_set ::
    FunPtr (CInt -> CString -> Ptr () -> IO ()) -> -- log_callback
    Ptr () -> -- user_data
    IO ()
