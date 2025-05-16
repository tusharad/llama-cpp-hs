module Llama.Internal.Foreign.Tokenize where

import Foreign
import Foreign.C
import Llama.Internal.Types

{- | LLAMA_API int32_t llama_tokenize(
|     const struct llama_vocab * vocab,
|     const char * text,
|     int32_t text_len,
|     llama_token * tokens,
|     int32_t n_tokens_max,
|     bool add_special,
|     bool parse_special);
-}
foreign import ccall "llama_tokenize"
  c_llama_tokenize ::
    CLlamaVocab -> -- vocab
    CString -> -- text
    CInt -> -- text_len
    Ptr LlamaToken -> -- tokens
    CInt -> -- n_tokens_max
    CBool -> -- add_special
    CBool -> -- parse_special
    IO CInt

{- | LLAMA_API int32_t llama_token_to_piece(
|     const struct llama_vocab * vocab,
|     llama_token token,
|     char * buf,
|     int32_t length,
|     int32_t lstrip,
|     bool special);
-}
foreign import ccall "llama_token_to_piece"
  c_llama_token_to_piece ::
    CLlamaVocab -> -- vocab
    LlamaToken -> -- token
    CString -> -- buf
    CInt -> -- length
    CInt -> -- lstrip
    CBool -> -- special
    IO CInt

{- | LLAMA_API int32_t llama_detokenize(
|     const struct llama_vocab * vocab,
|     const llama_token * tokens,
|     int32_t n_tokens,
|     char * text,
|     int32_t text_len_max,
|     bool remove_special,
|     bool unparse_special);
-}
foreign import ccall "llama_detokenize"
  c_llama_detokenize ::
    CLlamaVocab -> -- vocab
    Ptr LlamaToken -> -- tokens
    CInt -> -- n_tokens
    CString -> -- text
    CInt -> -- text_len_max
    CBool -> -- remove_special
    CBool -> -- unparse_special
    IO CInt
