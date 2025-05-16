module Llama.Internal.Foreign.Sampler (
  c_llama_sampler_init_greedy
  , c_llama_sampler_init_dist
  , c_llama_sampler_init_top_k
  , c_llama_sampler_init_top_p
  , c_llama_sampler_init_min_p
  , c_llama_sampler_init_typical
  , c_llama_sampler_init_temp
  , c_llama_sampler_init_temp_ext
  , c_llama_sampler_init_xtc
  , c_llama_sampler_init_top_n_sigma
  , c_llama_sampler_init_mirostat
  , c_llama_sampler_init_mirostat_v2
  , c_llama_sampler_init_grammar
  , c_llama_sampler_init_grammar_lazy_patterns
  , c_llama_sampler_init_penalties
  , c_llama_sampler_init_dry
  , c_llama_sampler_init_logit_bias
  , c_llama_sampler_init_infill
  , c_llama_sampler_sample
  , c_llama_sampler_get_seed
  , c_llama_sampler_chain_default_params_into
  , c_llama_sampler_init
  , c_llama_sampler_name
  , c_llama_sampler_accept
  , c_llama_sampler_apply
  , c_llama_sampler_reset
  , c_llama_sampler_clone
  , p_llama_sampler_free
  , c_llama_sampler_free
  , c_llama_sampler_chain_init
  , c_llama_sampler_chain_add
  , c_llama_sampler_chain_get
  , c_llama_sampler_chain_n
  , c_llama_sampler_chain_remove
) where

import Foreign
import Foreign.C
import Llama.Internal.Types
import Llama.Internal.Types.Params

foreign import ccall "llama_sampler_chain_default_params_into"
  c_llama_sampler_chain_default_params_into :: Ptr LlamaSamplerChainParams -> IO ()

-- | LLAMA_API struct llama_sampler * llama_sampler_init(const struct llama_sampler_i * iface, llama_sampler_context_t ctx);
foreign import ccall "llama_sampler_init"
  c_llama_sampler_init ::
    Ptr LlamaSamplerI -> -- iface (vtable)
    LlamaSamplerContext -> -- ctx (opaque context)
    IO (Ptr LlamaSampler)

-- | LLAMA_API const char * llama_sampler_name(const struct llama_sampler * smpl);
foreign import ccall "llama_sampler_name"
  c_llama_sampler_name ::
    Ptr LlamaSampler -> -- smpl
    IO CString

-- | LLAMA_API void llama_sampler_accept(struct llama_sampler * smpl, llama_token token);
foreign import ccall "llama_sampler_accept"
  c_llama_sampler_accept ::
    Ptr LlamaSampler -> -- smpl
    LlamaToken -> -- token
    IO ()

-- | LLAMA_API void llama_sampler_apply(struct llama_sampler * smpl, llama_token_data_array * cur_p);
foreign import ccall "llama_sampler_apply"
  c_llama_sampler_apply ::
    Ptr LlamaSampler -> -- smpl
    Ptr LlamaTokenDataArray -> -- cur_p
    IO ()

-- | LLAMA_API void llama_sampler_reset(struct llama_sampler * smpl);
foreign import ccall "llama_sampler_reset"
  c_llama_sampler_reset ::
    Ptr LlamaSampler -> -- smpl
    IO ()

-- | LLAMA_API struct llama_sampler * llama_sampler_clone(const struct llama_sampler * smpl);
foreign import ccall "llama_sampler_clone"
  c_llama_sampler_clone ::
    Ptr LlamaSampler -> -- smpl
    IO (Ptr LlamaSampler)

-- | Foreign pointer to the C function for freeing the SamplerChain.
foreign import ccall "&llama_sampler_free"
  p_llama_sampler_free :: FinalizerPtr LlamaSampler

-- | LLAMA_API void llama_sampler_free(struct llama_sampler * smpl);
foreign import ccall "llama_sampler_free"
  c_llama_sampler_free ::
    Ptr LlamaSampler -> -- smpl
    IO ()

-- | LLAMA_API struct llama_sampler * llama_sampler_chain_init(struct llama_sampler_chain_params params);
foreign import ccall "llama_sampler_chain_init"
  c_llama_sampler_chain_init ::
    LlamaSamplerChainParams -> -- params
    IO (Ptr LlamaSampler)

-- | LLAMA_API void llama_sampler_chain_add(struct llama_sampler * chain, struct llama_sampler * smpl);
foreign import ccall "llama_sampler_chain_add"
  c_llama_sampler_chain_add ::
    Ptr LlamaSampler -> -- chain
    Ptr LlamaSampler -> -- smpl
    IO ()

-- | LLAMA_API struct llama_sampler * llama_sampler_chain_get(const struct llama_sampler * chain, int32_t i);
foreign import ccall "llama_sampler_chain_get"
  c_llama_sampler_chain_get ::
    Ptr LlamaSampler -> -- chain
    CInt -> -- i
    IO (Ptr LlamaSampler)

-- | LLAMA_API int llama_sampler_chain_n(const struct llama_sampler * chain);
foreign import ccall "llama_sampler_chain_n"
  c_llama_sampler_chain_n ::
    Ptr LlamaSampler -> -- chain
    IO CInt

-- | LLAMA_API struct llama_sampler * llama_sampler_chain_remove(struct llama_sampler * chain, int32_t i);
foreign import ccall "llama_sampler_chain_remove"
  c_llama_sampler_chain_remove ::
    Ptr LlamaSampler -> -- chain
    CInt -> -- i
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_greedy(void);
foreign import ccall "llama_sampler_init_greedy"
  c_llama_sampler_init_greedy ::
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_dist(uint32_t seed);
foreign import ccall "llama_sampler_init_dist"
  c_llama_sampler_init_dist ::
    CUInt -> -- seed
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_top_k(int32_t k);
foreign import ccall "llama_sampler_init_top_k"
  c_llama_sampler_init_top_k ::
    CInt -> -- k
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_top_p(float p, size_t min_keep);
foreign import ccall "llama_sampler_init_top_p"
  c_llama_sampler_init_top_p ::
    CFloat -> -- p
    CSize -> -- min_keep
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_min_p(float p, size_t min_keep);
foreign import ccall "llama_sampler_init_min_p"
  c_llama_sampler_init_min_p ::
    CFloat -> -- p
    CSize -> -- min_keep
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_typical(float p, size_t min_keep);
foreign import ccall "llama_sampler_init_typical"
  c_llama_sampler_init_typical ::
    CFloat -> -- p
    CSize -> -- min_keep
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_temp(float t);
foreign import ccall "llama_sampler_init_temp"
  c_llama_sampler_init_temp ::
    CFloat -> -- t
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_temp_ext(float t, float delta, float exponent);
foreign import ccall "llama_sampler_init_temp_ext"
  c_llama_sampler_init_temp_ext ::
    CFloat -> -- t
    CFloat -> -- delta
    CFloat -> -- exponent
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_xtc(float p, float t, size_t min_keep, uint32_t seed);
foreign import ccall "llama_sampler_init_xtc"
  c_llama_sampler_init_xtc ::
    CFloat -> -- p
    CFloat -> -- t
    CSize -> -- min_keep
    CUInt -> -- seed
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_top_n_sigma(float n);
foreign import ccall "llama_sampler_init_top_n_sigma"
  c_llama_sampler_init_top_n_sigma ::
    CFloat -> -- n
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_mirostat(int32_t n_vocab, uint32_t seed, float tau, float eta, int32_t m);
foreign import ccall "llama_sampler_init_mirostat"
  c_llama_sampler_init_mirostat ::
    CInt -> -- n_vocab
    CUInt -> -- seed
    CFloat -> -- tau
    CFloat -> -- eta
    CInt -> -- m
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_mirostat_v2(uint32_t seed, float tau, float eta);
foreign import ccall "llama_sampler_init_mirostat_v2"
  c_llama_sampler_init_mirostat_v2 ::
    CUInt -> -- seed
    CFloat -> -- tau
    CFloat -> -- eta
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_grammar(const struct llama_vocab * vocab, const char * grammar_str, const char * grammar_root);
foreign import ccall "llama_sampler_init_grammar"
  c_llama_sampler_init_grammar ::
    CLlamaVocab -> -- vocab
    CString -> -- grammar_str
    CString -> -- grammar_root
    IO (Ptr LlamaSampler)

{- | LLAMA_API struct llama_sampler * llama_sampler_init_grammar_lazy_patterns(
|         const struct llama_vocab * vocab,
|         const char * grammar_str,
|         const char * grammar_root,
|         const char ** trigger_patterns,
|         size_t num_trigger_patterns,
|         const llama_token * trigger_tokens,
|         size_t num_trigger_tokens);
-}
foreign import ccall "llama_sampler_init_grammar_lazy_patterns"
  c_llama_sampler_init_grammar_lazy_patterns ::
    CLlamaVocab -> -- vocab
    CString -> -- grammar_str
    CString -> -- grammar_root
    Ptr CString -> -- trigger_patterns (array of CStrings)
    CSize -> -- num_trigger_patterns
    Ptr LlamaToken -> -- trigger_tokens
    CSize -> -- num_trigger_tokens
    IO (Ptr LlamaSampler)

{- | LLAMA_API struct llama_sampler * llama_sampler_init_penalties(
|         int32_t penalty_last_n,
|         float penalty_repeat,
|         float penalty_freq,
|         float penalty_present);
-}
foreign import ccall "llama_sampler_init_penalties"
  c_llama_sampler_init_penalties ::
    CInt -> -- penalty_last_n
    CFloat -> -- penalty_repeat
    CFloat -> -- penalty_freq
    CFloat -> -- penalty_present
    IO (Ptr LlamaSampler)

{- | LLAMA_API struct llama_sampler * llama_sampler_init_dry(
|         const struct llama_vocab * vocab,
|         int32_t n_ctx_train,
|         float dry_multiplier,
|         float dry_base,
|         int32_t dry_allowed_length,
|         int32_t dry_penalty_last_n,
|         const char ** seq_breakers,
|         size_t num_breakers);
-}
foreign import ccall "llama_sampler_init_dry"
  c_llama_sampler_init_dry ::
    CLlamaVocab -> -- vocab
    CInt -> -- n_ctx_train
    CFloat -> -- dry_multiplier
    CFloat -> -- dry_base
    CInt -> -- dry_allowed_length
    CInt -> -- dry_penalty_last_n
    Ptr CString -> -- seq_breakers (array of CStrings)
    CSize -> -- num_breakers
    IO (Ptr LlamaSampler)

{- | LLAMA_API struct llama_sampler * llama_sampler_init_logit_bias(
|         int32_t n_vocab,
|         int32_t n_logit_bias,
|         const llama_logit_bias * logit_bias);
-}
foreign import ccall "llama_sampler_init_logit_bias"
  c_llama_sampler_init_logit_bias ::
    CInt -> -- n_vocab
    CInt -> -- n_logit_bias
    Ptr LlamaLogitBias -> -- logit_bias (opaque struct)
    IO (Ptr LlamaSampler)

-- | LLAMA_API struct llama_sampler * llama_sampler_init_infill(const struct llama_vocab * vocab);
foreign import ccall "llama_sampler_init_infill"
  c_llama_sampler_init_infill ::
    CLlamaVocab -> -- vocab
    IO (Ptr LlamaSampler)

-- | LLAMA_API uint32_t llama_sampler_get_seed(const struct llama_sampler * smpl);
foreign import ccall "llama_sampler_get_seed"
  c_llama_sampler_get_seed ::
    Ptr LlamaSampler -> -- smpl

    -- | Returns seed used by the sampler
    IO CUInt

-- | LLAMA_API llama_token llama_sampler_sample(struct llama_sampler * smpl, struct llama_context * ctx, int32_t idx);
foreign import ccall "llama_sampler_sample"
  c_llama_sampler_sample ::
    Ptr LlamaSampler -> -- smpl
    CLlamaContext -> -- ctx
    CInt -> -- idx
    IO LlamaToken
