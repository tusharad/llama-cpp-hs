module Llama.Internal.Foreign.ChatTemplate
  ( c_llama_chat_apply_template
  , c_llama_chat_builtin_templates
  ) where

import Foreign
import Foreign.C
import Llama.Internal.Types

{- | LLAMA_API int32_t llama_chat_apply_template(
|         const char * tmpl,
|         const struct llama_chat_message * chat,
|         size_t n_msg,
|         bool add_ass,
|         char * buf,
|         int32_t length);
-}
foreign import ccall "llama_chat_apply_template"
  c_llama_chat_apply_template ::
    CString -> -- tmpl (nullable)
    Ptr LlamaChatMessage -> -- chat (array of messages)
    CSize -> -- n_msg (number of messages)
    CBool -> -- add_ass (add assistant token)
    CString -> -- buf (output buffer)
    CInt -> -- length (size of buffer)

    -- | Returns required buffer size (if > length) or bytes written
    IO CInt

-- | LLAMA_API int32_t llama_chat_builtin_templates(const char ** output, size_t len);
foreign import ccall "llama_chat_builtin_templates"
  c_llama_chat_builtin_templates ::
    Ptr CString -> -- output (array of template names)
    CSize -> -- len (max number of names to write)

    -- | Returns total number of available templates
    IO CInt
