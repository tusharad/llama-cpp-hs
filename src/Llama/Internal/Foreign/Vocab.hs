module Llama.Internal.Foreign.Vocab where

import Foreign.C
import Llama.Internal.Types

--LLAMA_API int32_t llama_n_vocab    (const struct llama_vocab * vocab)

foreign import ccall "llama_n_vocab" c_llama_n_vocab :: CLlamaVocab -> IO CInt

foreign import ccall "llama_vocab_n_tokens"
  c_llama_vocab_n_tokens ::
    CLlamaVocab -> IO CInt

-- | LLAMA_API const char * llama_vocab_get_text(const struct llama_vocab * vocab, llama_token token);
foreign import ccall "llama_vocab_get_text"
  c_llama_vocab_get_text ::
    CLlamaVocab -> -- vocab
    LlamaToken -> -- token
    IO CString

-- | LLAMA_API float llama_vocab_get_score(const struct llama_vocab * vocab, llama_token token);
foreign import ccall "llama_vocab_get_score"
  c_llama_vocab_get_score ::
    CLlamaVocab -> -- vocab
    LlamaToken -> -- token
    IO CFloat

-- | LLAMA_API enum llama_token_attr llama_vocab_get_attr(const struct llama_vocab * vocab, llama_token token);
foreign import ccall "llama_vocab_get_attr"
  c_llama_vocab_get_attr ::
    CLlamaVocab -> -- vocab
    LlamaToken -> -- token

    -- | Returns raw integer representing enum value
    IO CInt

-- | LLAMA_API bool llama_vocab_is_eog(const struct llama_vocab * vocab, llama_token token);
foreign import ccall "llama_vocab_is_eog"
  c_llama_vocab_is_eog ::
    CLlamaVocab -> -- vocab
    LlamaToken -> -- token
    IO CBool

-- | LLAMA_API bool llama_vocab_is_control(const struct llama_vocab * vocab, llama_token token);
foreign import ccall "llama_vocab_is_control"
  c_llama_vocab_is_control ::
    CLlamaVocab -> -- vocab
    LlamaToken -> -- token
    IO CBool

-- | LLAMA_API llama_token llama_vocab_bos(const struct llama_vocab * vocab); // beginning-of-sentence
foreign import ccall "llama_vocab_bos"
  c_llama_vocab_bos ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_eos(const struct llama_vocab * vocab); // end-of-sentence
foreign import ccall "llama_vocab_eos"
  c_llama_vocab_eos ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_eot(const struct llama_vocab * vocab); // end-of-turn
foreign import ccall "llama_vocab_eot"
  c_llama_vocab_eot ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_sep(const struct llama_vocab * vocab); // sentence separator
foreign import ccall "llama_vocab_sep"
  c_llama_vocab_sep ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_nl (const struct llama_vocab * vocab); // next-line
foreign import ccall "llama_vocab_nl"
  c_llama_vocab_nl ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_pad(const struct llama_vocab * vocab); // padding
foreign import ccall "llama_vocab_pad"
  c_llama_vocab_pad ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API bool llama_vocab_get_add_bos(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_get_add_bos"
  c_llama_vocab_get_add_bos ::
    CLlamaVocab -> -- vocab
    IO CBool

-- | LLAMA_API bool llama_vocab_get_add_eos(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_get_add_eos"
  c_llama_vocab_get_add_eos ::
    CLlamaVocab -> -- vocab
    IO CBool

-- | LLAMA_API llama_token llama_vocab_fim_pre(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_fim_pre"
  c_llama_vocab_fim_pre ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_fim_suf(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_fim_suf"
  c_llama_vocab_fim_suf ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_fim_mid(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_fim_mid"
  c_llama_vocab_fim_mid ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_fim_pad(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_fim_pad"
  c_llama_vocab_fim_pad ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_fim_rep(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_fim_rep"
  c_llama_vocab_fim_rep ::
    CLlamaVocab -> -- vocab
    IO LlamaToken

-- | LLAMA_API llama_token llama_vocab_fim_sep(const struct llama_vocab * vocab);
foreign import ccall "llama_vocab_fim_sep"
  c_llama_vocab_fim_sep ::
    CLlamaVocab -> -- vocab
    IO LlamaToken
