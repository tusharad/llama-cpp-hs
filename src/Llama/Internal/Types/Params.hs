{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Llama.Internal.Types.Params
  ( LlamaSamplerChainParams (..)
  , LlamaContextParams (..)
  , LlamaModelParams (..)
  , LlamaModelQuantizeParams (..)
  , GgmlType (..)
  , LlamaRopeTypeScaling (..)
  , LlamaPoolingType (..)
  , LlamaAttentionType (..)
  , LlamaSplitMode (..)
  , CLlamaContextParams (..)
  , CLlamaModelParams (..)
  , CLlamaSamplerChainParams (..)
  , CLlamaModelQuantizeParams (..)
  , SamplerChainParamsPtr (..)
  , ModelQuantizeParamsPtr (..)
  , ContextParamsPtr (..)
  , ModelParamsPtr (..)
  , LlamaVocabType (..)
  , fromLlamaRopePoolingType
  , fromLlamaRopeTypeScaling
  , fromLlamaRopeVocabType 
  ) where

import Foreign
import Foreign.C.Types
import Foreign.Storable.Generic
import GHC.Generics

-- | Raw pointer to llama_sampler_chain_params
newtype CLlamaSamplerChainParams = CLlamaSamplerChainParams (Ptr LlamaSamplerChainParams)
  deriving (Show, Eq)

-- | Raw pointer to llama_model_quantize_params
newtype CLlamaModelQuantizeParams = CLlamaModelQuantizeParams (Ptr LlamaModelQuantizeParams)
  deriving (Show, Eq)

-- | Raw pointer to llama_context_params
newtype CLlamaContextParams = CLlamaContextParams (Ptr LlamaContextParams)
  deriving (Show, Eq)

-- | Raw pointer to llama_model_params
newtype CLlamaModelParams = CLlamaModelParams (Ptr LlamaModelParams)
  deriving (Show, Eq)

-- | Safe wrapper for managed 'llama_sampler_chain_params'
newtype SamplerChainParamsPtr = SamplerChainParamsPtr (ForeignPtr CLlamaSamplerChainParams)
  deriving (Show, Eq)

-- | Safe wrapper for managed 'llama_model_quantize_params'
newtype ModelQuantizeParamsPtr = ModelQuantizeParamsPtr (ForeignPtr CLlamaModelQuantizeParams)
  deriving (Show, Eq)

-- | Safe wrapper for managed 'llama_context_params'
newtype ContextParamsPtr = ContextParamsPtr (ForeignPtr CLlamaContextParams)
  deriving (Show, Eq)

-- | Safe wrapper for managed 'llama_model_params'
newtype ModelParamsPtr = ModelParamsPtr (ForeignPtr CLlamaModelParams)
  deriving (Show, Eq)

-- Forward declarations for opaque C structs
data GgmlBackendDevT
data LlamaModelTensorBuftOverride
data LlamaModelKvOverride

-- Enum for llama_split_mode (assumed to be a 32-bit integer)
newtype LlamaSplitMode = LlamaSplitMode Int32
  deriving (Eq, Show, Read, Generic, GStorable)

-- Define the sampler chain parameters structure
newtype LlamaSamplerChainParams = LlamaSamplerChainParams
  { noPerf :: CBool -- whether to measure performance timings
  }
  deriving (Show, Eq, Generic, GStorable)

-- model quantization parameters
data LlamaModelQuantizeParams = LlamaModelQuantizeParams
  { nthread :: Int32
  , ftype :: LlamaFtype
  , outputTensorType :: GgmlType
  , tokenEmbeddingType :: GgmlType
  , allowRequantize :: Bool
  , quantizeOutputTensor :: Bool
  , onlyCopy :: Bool
  , pure_ :: Bool
  , keepSplit :: Bool
  , imatrix :: Ptr ()
  , kvOverridesQuantizeParams :: Ptr ()
  , tensorTypes :: Ptr ()
  }
  deriving (Show, Eq, Generic, GStorable)

data LlamaContextParams = LlamaContextParams
  { n_ctx :: CUInt
  -- ^ text context, 0 = from model
  , n_batch :: CUInt
  -- ^ logical maximum batch size that can be submitted to llama_decode
  , n_ubatch :: CUInt
  -- ^ physical maximum batch size
  , n_seq_max :: CUInt
  -- ^ max number of sequences (i.e. distinct states for recurrent models)
  , n_threads :: CInt
  -- ^ number of threads to use for generation
  , n_threads_batch :: CInt
  -- ^ number of threads to use for batch processing
  , rope_scaling_type :: LlamaRopeTypeScaling
  -- ^ RoPE scaling type, from `enum llama_rope_scaling_type`
  , pooling_type :: LlamaPoolingType
  -- ^ whether to pool (sum) embedding results by sequence id
  , attention_type :: LlamaAttentionType
  -- ^ attention type to use for embeddings
  , rope_freq_base :: CFloat
  -- ^ RoPE base frequency, 0 = from model
  , rope_freq_scale :: CFloat
  -- ^ RoPE frequency scaling factor, 0 = from model
  , yarn_ext_factor :: CFloat
  -- ^ YaRN extrapolation mix factor, negative = from model
  , yarn_attn_factor :: CFloat
  -- ^ YaRN magnitude scaling factor
  , yarn_beta_fast :: CFloat
  -- ^ YaRN low correction dim
  , yarn_beta_slow :: CFloat
  -- ^ YaRN high correction dim
  , yarn_orig_ctx :: CUInt
  -- ^ YaRN original context size
  , defrag_thold :: CFloat
  -- ^ defragment the KV cache if holes/size > thold, < 0 disabled (default)
  , cb_eval :: FunPtr (Ptr () -> IO ())
  , cb_eval_user_data :: Ptr ()
  , type_k :: GgmlType
  -- ^ data type for K cache [EXPERIMENTAL]
  , type_v :: GgmlType
  -- ^ data type for V cache [EXPERIMENTAL]
  , logits_all :: CBool
  -- ^ the llama_decode() call computes all logits, not just the last one (DEPRECATED - set llama_batch.logits instead)
  , embeddings :: CBool
  -- ^ if true, extract embeddings (together with logits)
  , offload_kqv :: CBool
  -- ^ whether to offload the KQV ops (including the KV cache) to GPU
  , flash_attn :: CBool
  -- ^ whether to use flash attention [EXPERIMENTAL]
  , no_perf :: CBool
  -- ^ whether to measure performance timings
  , abort_callback :: FunPtr (Ptr () -> IO CInt)
  , abort_callback_data :: Ptr ()
  }
  deriving (Show, Generic, GStorable)

-- Haskell representation of the C struct llama_model_params
data LlamaModelParams = LlamaModelParams
  { devices :: Ptr GgmlBackendDevT
  -- ^ NULL-terminated list of devices for offloading
  , tensorBuftOverrides :: Ptr LlamaModelTensorBuftOverride
  -- ^ NULL-terminated list of buffer type overrides
  , nGpuLayers :: Int32
  -- ^ Number of layers to store in VRAM
  , splitMode :: LlamaSplitMode
  -- ^ How to split the model across GPUs
  , mainGpu :: Int32
  -- ^ GPU used when split_mode is LLAMA_SPLIT_MODE_NONE
  , tensorSplit :: Ptr CFloat
  -- ^ Proportion of model offloaded to each GPU
  , progressCallback :: FunPtr (CFloat -> Ptr () -> IO CBool)
  -- ^ Callback for progress updates (returns true to continue)
  , progressCallbackUserData :: Ptr ()
  -- ^ User data for progress callback
  , kvOverrides :: Ptr LlamaModelKvOverride
  -- ^ Override for model metadata
  , vocabOnly :: CBool
  -- ^ Only load vocabulary, not weights
  , useMmap :: CBool
  -- ^ Use mmap if possible
  , useMlock :: CBool
  -- ^ Force model to stay in RAM
  , checkTensors :: CBool
  -- ^ Validate tensor data
  }
  deriving (Generic, GStorable, Eq, Show)

-- Helpers
data LlamaRopeTypeScaling
  = LLAMA_ROPE_SCALING_TYPE_UNSPECIFIED
  | LLAMA_ROPE_SCALING_TYPE_NONE
  | LLAMA_ROPE_SCALING_TYPE_LINEAR
  | LLAMA_ROPE_SCALING_TYPE_YARN
  | LLAMA_ROPE_SCALING_TYPE_LONGROPE
  | LLAMA_ROPE_SCALING_TYPE_MAX_VALUE
  deriving (Show, Eq)

toLlamaRopeTypeScaling :: LlamaRopeTypeScaling -> CInt
toLlamaRopeTypeScaling LLAMA_ROPE_SCALING_TYPE_UNSPECIFIED = -1
toLlamaRopeTypeScaling LLAMA_ROPE_SCALING_TYPE_NONE = 0
toLlamaRopeTypeScaling LLAMA_ROPE_SCALING_TYPE_LINEAR = 1
toLlamaRopeTypeScaling LLAMA_ROPE_SCALING_TYPE_YARN = 2
toLlamaRopeTypeScaling LLAMA_ROPE_SCALING_TYPE_LONGROPE = 3
toLlamaRopeTypeScaling LLAMA_ROPE_SCALING_TYPE_MAX_VALUE = 3

fromLlamaRopeTypeScaling :: CInt -> Maybe LlamaRopeTypeScaling
fromLlamaRopeTypeScaling (-1) = Just LLAMA_ROPE_SCALING_TYPE_UNSPECIFIED
fromLlamaRopeTypeScaling 0 = Just LLAMA_ROPE_SCALING_TYPE_NONE
fromLlamaRopeTypeScaling 1 = Just LLAMA_ROPE_SCALING_TYPE_LINEAR
fromLlamaRopeTypeScaling 2 = Just LLAMA_ROPE_SCALING_TYPE_YARN
fromLlamaRopeTypeScaling 3 = Just LLAMA_ROPE_SCALING_TYPE_LONGROPE
fromLlamaRopeTypeScaling _ = Nothing

instance Storable LlamaRopeTypeScaling where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    mbVal <- fromLlamaRopeTypeScaling <$> peek (castPtr ptr)
    case mbVal of
      Nothing -> error "Invalid LlamaRopeTypeScaling value"
      Just val -> return val
  poke ptr val = do
    let val' = toLlamaRopeTypeScaling val
    poke (castPtr ptr) val'

data LlamaPoolingType
  = LLAMA_POOLING_TYPE_UNSPECIFIED
  | LLAMA_POOLING_TYPE_NONE
  | LLAMA_POOLING_TYPE_MEAN
  | LLAMA_POOLING_TYPE_CLS
  | LLAMA_POOLING_TYPE_LAST
  | LLAMA_POOLING_TYPE_RANK
  deriving (Show, Eq, Ord, Generic)

instance Storable LlamaPoolingType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    mbVal <- fromLlamaRopePoolingType <$> peek (castPtr ptr)
    case mbVal of
      Nothing -> error "Invalid LlamaPoolingType value"
      Just val -> return val
  poke ptr val = do
    let val' = toLlamaRopePoolingType val
    poke (castPtr ptr) val'

toLlamaRopePoolingType :: LlamaPoolingType -> CInt
toLlamaRopePoolingType LLAMA_POOLING_TYPE_UNSPECIFIED = -1
toLlamaRopePoolingType LLAMA_POOLING_TYPE_NONE = 0
toLlamaRopePoolingType LLAMA_POOLING_TYPE_MEAN = 1
toLlamaRopePoolingType LLAMA_POOLING_TYPE_CLS = 2
toLlamaRopePoolingType LLAMA_POOLING_TYPE_LAST = 3
toLlamaRopePoolingType LLAMA_POOLING_TYPE_RANK = 4

fromLlamaRopePoolingType :: CInt -> Maybe LlamaPoolingType
fromLlamaRopePoolingType (-1) = Just LLAMA_POOLING_TYPE_UNSPECIFIED
fromLlamaRopePoolingType 0 = Just LLAMA_POOLING_TYPE_NONE
fromLlamaRopePoolingType 1 = Just LLAMA_POOLING_TYPE_MEAN
fromLlamaRopePoolingType 2 = Just LLAMA_POOLING_TYPE_CLS
fromLlamaRopePoolingType 3 = Just LLAMA_POOLING_TYPE_LAST
fromLlamaRopePoolingType 4 = Just LLAMA_POOLING_TYPE_RANK
fromLlamaRopePoolingType _ = Nothing

data LlamaAttentionType
  = LLAMA_ATTENTION_TYPE_UNSPECIFIED
  | LLAMA_ATTENTION_TYPE_CAUSAL
  | LLAMA_ATTENTION_TYPE_NON_CAUSAL
  deriving (Show, Eq, Ord, Generic)

instance Storable LlamaAttentionType where
  sizeOf _ = sizeOf (undefined :: CUInt)

  alignment _ = sizeOf (undefined :: CUInt)

  peek ptr = do
    mbVal <- fromLlamaAttentionType <$> peek (castPtr ptr)
    case mbVal of
      Nothing -> error "Invalid LlamaAttentionType value"
      Just val -> return val
  poke ptr val = do
    let val' = toLlamaAttentionType val
    poke (castPtr ptr) val'

toLlamaAttentionType :: LlamaAttentionType -> CInt
toLlamaAttentionType LLAMA_ATTENTION_TYPE_UNSPECIFIED = -1
toLlamaAttentionType LLAMA_ATTENTION_TYPE_CAUSAL = 0
toLlamaAttentionType LLAMA_ATTENTION_TYPE_NON_CAUSAL = 1

fromLlamaAttentionType :: CInt -> Maybe LlamaAttentionType
fromLlamaAttentionType (-1) = Just LLAMA_ATTENTION_TYPE_UNSPECIFIED
fromLlamaAttentionType 0 = Just LLAMA_ATTENTION_TYPE_CAUSAL
fromLlamaAttentionType 1 = Just LLAMA_ATTENTION_TYPE_NON_CAUSAL
fromLlamaAttentionType _ = Nothing

data GgmlType
  = GGML_TYPE_F32
  | GGML_TYPE_F16
  | GGML_TYPE_Q4_0
  | GGML_TYPE_Q4_1
  | GGML_TYPE_Q5_0
  | GGML_TYPE_Q5_1
  | GGML_TYPE_Q8_0
  | GGML_TYPE_Q8_1
  | GGML_TYPE_Q2_K
  | GGML_TYPE_Q3_K
  | GGML_TYPE_Q4_K
  | GGML_TYPE_Q5_K
  | GGML_TYPE_Q6_K
  | GGML_TYPE_Q8_K
  | GGML_TYPE_IQ2_XXS
  | GGML_TYPE_IQ2_XS
  | GGML_TYPE_IQ3_XXS
  | GGML_TYPE_IQ1_S
  | GGML_TYPE_IQ4_NL
  | GGML_TYPE_IQ3_S
  | GGML_TYPE_IQ2_S
  | GGML_TYPE_IQ4_XS
  | GGML_TYPE_I8
  | GGML_TYPE_I16
  | GGML_TYPE_I32
  | GGML_TYPE_I64
  | GGML_TYPE_F64
  | GGML_TYPE_IQ1_M
  | GGML_TYPE_BF16
  | GGML_TYPE_TQ1_0
  | GGML_TYPE_TQ2_0
  | GGML_TYPE_COUNT
  deriving (Show, Eq, Ord, Generic)

toGgmlType :: GgmlType -> CInt
toGgmlType g =
  case g of
    GGML_TYPE_F32 -> 0
    GGML_TYPE_F16 -> 1
    GGML_TYPE_Q4_0 -> 2
    GGML_TYPE_Q4_1 -> 3
    GGML_TYPE_Q5_0 -> 6
    GGML_TYPE_Q5_1 -> 7
    GGML_TYPE_Q8_0 -> 8
    GGML_TYPE_Q8_1 -> 9
    GGML_TYPE_Q2_K -> 10
    GGML_TYPE_Q3_K -> 11
    GGML_TYPE_Q4_K -> 12
    GGML_TYPE_Q5_K -> 13
    GGML_TYPE_Q6_K -> 14
    GGML_TYPE_Q8_K -> 15
    GGML_TYPE_IQ2_XXS -> 16
    GGML_TYPE_IQ2_XS -> 17
    GGML_TYPE_IQ3_XXS -> 18
    GGML_TYPE_IQ1_S -> 19
    GGML_TYPE_IQ4_NL -> 20
    GGML_TYPE_IQ3_S -> 21
    GGML_TYPE_IQ2_S -> 22
    GGML_TYPE_IQ4_XS -> 23
    GGML_TYPE_I8 -> 24
    GGML_TYPE_I16 -> 25
    GGML_TYPE_I32 -> 26
    GGML_TYPE_I64 -> 27
    GGML_TYPE_F64 -> 28
    GGML_TYPE_IQ1_M -> 29
    GGML_TYPE_BF16 -> 30
    GGML_TYPE_TQ1_0 -> 34
    GGML_TYPE_TQ2_0 -> 35
    GGML_TYPE_COUNT -> 39

fromGgmlType :: CInt -> Maybe GgmlType
fromGgmlType num =
  case num of
    0 -> Just GGML_TYPE_F32
    1 -> Just GGML_TYPE_F16
    2 -> Just GGML_TYPE_Q4_0
    3 -> Just GGML_TYPE_Q4_1
    6 -> Just GGML_TYPE_Q5_0
    7 -> Just GGML_TYPE_Q5_1
    8 -> Just GGML_TYPE_Q8_0
    9 -> Just GGML_TYPE_Q8_1
    10 -> Just GGML_TYPE_Q2_K
    11 -> Just GGML_TYPE_Q3_K
    12 -> Just GGML_TYPE_Q4_K
    13 -> Just GGML_TYPE_Q5_K
    14 -> Just GGML_TYPE_Q6_K
    15 -> Just GGML_TYPE_Q8_K
    16 -> Just GGML_TYPE_IQ2_XXS
    17 -> Just GGML_TYPE_IQ2_XS
    18 -> Just GGML_TYPE_IQ3_XXS
    19 -> Just GGML_TYPE_IQ1_S
    20 -> Just GGML_TYPE_IQ4_NL
    21 -> Just GGML_TYPE_IQ3_S
    22 -> Just GGML_TYPE_IQ2_S
    23 -> Just GGML_TYPE_IQ4_XS
    24 -> Just GGML_TYPE_I8
    25 -> Just GGML_TYPE_I16
    26 -> Just GGML_TYPE_I32
    27 -> Just GGML_TYPE_I64
    28 -> Just GGML_TYPE_F64
    29 -> Just GGML_TYPE_IQ1_M
    30 -> Just GGML_TYPE_BF16
    34 -> Just GGML_TYPE_TQ1_0
    35 -> Just GGML_TYPE_TQ2_0
    39 -> Just GGML_TYPE_COUNT
    _ -> Nothing

instance Storable GgmlType where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = sizeOf (undefined :: CInt)
  peek ptr = do
    mbVal <- fromGgmlType <$> peek (castPtr ptr)
    case mbVal of
      Nothing -> error "Invalid GgmlType value"
      Just val -> return val
  poke ptr val = do
    let val' = toGgmlType val
    poke (castPtr ptr) val'

data LlamaFtype
  = LLAMA_FTYPE_ALL_F32
  | LLAMA_FTYPE_MOSTLY_F16
  | LLAMA_FTYPE_MOSTLY_Q4_0
  | LLAMA_FTYPE_MOSTLY_Q4_1
  | LLAMA_FTYPE_MOSTLY_Q8_0
  | LLAMA_FTYPE_MOSTLY_Q5_0
  | LLAMA_FTYPE_MOSTLY_Q5_1
  | LLAMA_FTYPE_MOSTLY_Q2_K
  | LLAMA_FTYPE_MOSTLY_Q3_K_S
  | LLAMA_FTYPE_MOSTLY_Q3_K_M
  | LLAMA_FTYPE_MOSTLY_Q3_K_L
  | LLAMA_FTYPE_MOSTLY_Q4_K_S
  | LLAMA_FTYPE_MOSTLY_Q4_K_M
  | LLAMA_FTYPE_MOSTLY_Q5_K_S
  | LLAMA_FTYPE_MOSTLY_Q5_K_M
  | LLAMA_FTYPE_MOSTLY_Q6_K
  | LLAMA_FTYPE_MOSTLY_IQ2_XXS
  | LLAMA_FTYPE_MOSTLY_IQ2_XS
  | LLAMA_FTYPE_MOSTLY_Q2_K_S
  | LLAMA_FTYPE_MOSTLY_IQ3_XS
  | LLAMA_FTYPE_MOSTLY_IQ3_XXS
  | LLAMA_FTYPE_MOSTLY_IQ1_S
  | LLAMA_FTYPE_MOSTLY_IQ4_NL
  | LLAMA_FTYPE_MOSTLY_IQ3_S
  | LLAMA_FTYPE_MOSTLY_IQ3_M
  | LLAMA_FTYPE_MOSTLY_IQ2_S
  | LLAMA_FTYPE_MOSTLY_IQ2_M
  | LLAMA_FTYPE_MOSTLY_IQ4_XS
  | LLAMA_FTYPE_MOSTLY_IQ1_M
  | LLAMA_FTYPE_MOSTLY_BF16
  | LLAMA_FTYPE_MOSTLY_TQ1_0
  | LLAMA_FTYPE_MOSTLY_TQ2_0
  | LLAMA_FTYPE_GUESSED
  deriving (Show, Eq)

toLlamaFtype :: LlamaFtype -> CInt
toLlamaFtype LLAMA_FTYPE_ALL_F32 = 0
toLlamaFtype LLAMA_FTYPE_MOSTLY_F16 = 1
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q4_0 = 2
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q4_1 = 3
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q8_0 = 7
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q5_0 = 8
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q5_1 = 9
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q2_K = 10
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q3_K_S = 11
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q3_K_M = 12
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q3_K_L = 13
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q4_K_S = 14
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q4_K_M = 15
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q5_K_S = 16
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q5_K_M = 17
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q6_K = 18
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ2_XXS = 19
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ2_XS = 20
toLlamaFtype LLAMA_FTYPE_MOSTLY_Q2_K_S = 21
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ3_XS = 22
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ3_XXS = 23
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ1_S = 24
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ4_NL = 25
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ3_S = 26
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ3_M = 27
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ2_S = 28
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ2_M = 29
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ4_XS = 30
toLlamaFtype LLAMA_FTYPE_MOSTLY_IQ1_M = 31
toLlamaFtype LLAMA_FTYPE_MOSTLY_BF16 = 32
toLlamaFtype LLAMA_FTYPE_MOSTLY_TQ1_0 = 36
toLlamaFtype LLAMA_FTYPE_MOSTLY_TQ2_0 = 37
toLlamaFtype LLAMA_FTYPE_GUESSED = 1024

fromLlamaFtype :: CInt -> Maybe LlamaFtype
fromLlamaFtype 0 = Just LLAMA_FTYPE_ALL_F32
fromLlamaFtype 1 = Just LLAMA_FTYPE_MOSTLY_F16
fromLlamaFtype 2 = Just LLAMA_FTYPE_MOSTLY_Q4_0
fromLlamaFtype 3 = Just LLAMA_FTYPE_MOSTLY_Q4_1
fromLlamaFtype 7 = Just LLAMA_FTYPE_MOSTLY_Q8_0
fromLlamaFtype 8 = Just LLAMA_FTYPE_MOSTLY_Q5_0
fromLlamaFtype 9 = Just LLAMA_FTYPE_MOSTLY_Q5_1
fromLlamaFtype 10 = Just LLAMA_FTYPE_MOSTLY_Q2_K
fromLlamaFtype 11 = Just LLAMA_FTYPE_MOSTLY_Q3_K_S
fromLlamaFtype 12 = Just LLAMA_FTYPE_MOSTLY_Q3_K_M
fromLlamaFtype 13 = Just LLAMA_FTYPE_MOSTLY_Q3_K_L
fromLlamaFtype 14 = Just LLAMA_FTYPE_MOSTLY_Q4_K_S
fromLlamaFtype 15 = Just LLAMA_FTYPE_MOSTLY_Q4_K_M
fromLlamaFtype 16 = Just LLAMA_FTYPE_MOSTLY_Q5_K_S
fromLlamaFtype 17 = Just LLAMA_FTYPE_MOSTLY_Q5_K_M
fromLlamaFtype 18 = Just LLAMA_FTYPE_MOSTLY_Q6_K
fromLlamaFtype 19 = Just LLAMA_FTYPE_MOSTLY_IQ2_XXS
fromLlamaFtype 20 = Just LLAMA_FTYPE_MOSTLY_IQ2_XS
fromLlamaFtype 21 = Just LLAMA_FTYPE_MOSTLY_Q2_K_S
fromLlamaFtype 22 = Just LLAMA_FTYPE_MOSTLY_IQ3_XS
fromLlamaFtype 23 = Just LLAMA_FTYPE_MOSTLY_IQ3_XXS
fromLlamaFtype 24 = Just LLAMA_FTYPE_MOSTLY_IQ1_S
fromLlamaFtype 25 = Just LLAMA_FTYPE_MOSTLY_IQ4_NL
fromLlamaFtype 26 = Just LLAMA_FTYPE_MOSTLY_IQ3_S
fromLlamaFtype 27 = Just LLAMA_FTYPE_MOSTLY_IQ3_M
fromLlamaFtype 28 = Just LLAMA_FTYPE_MOSTLY_IQ2_S
fromLlamaFtype 29 = Just LLAMA_FTYPE_MOSTLY_IQ2_M
fromLlamaFtype 30 = Just LLAMA_FTYPE_MOSTLY_IQ4_XS
fromLlamaFtype 31 = Just LLAMA_FTYPE_MOSTLY_IQ1_M
fromLlamaFtype 32 = Just LLAMA_FTYPE_MOSTLY_BF16
fromLlamaFtype 36 = Just LLAMA_FTYPE_MOSTLY_TQ1_0
fromLlamaFtype 37 = Just LLAMA_FTYPE_MOSTLY_TQ2_0
fromLlamaFtype 1024 = Just LLAMA_FTYPE_GUESSED
fromLlamaFtype _ = Nothing

instance Storable LlamaFtype where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    val <- peek (castPtr ptr) :: IO CInt
    case fromLlamaFtype val of
      Just v -> return v
      Nothing -> error $ "Invalid LlamaFtype value: " ++ show val
  poke ptr val = do
    let cVal = toLlamaFtype val
    poke (castPtr ptr) cVal

data LlamaVocabType
  = LLAMA_VOCAB_TYPE_NONE
  | LLAMA_VOCAB_TYPE_SPM
  | LLAMA_VOCAB_TYPE_BPE
  | LLAMA_VOCAB_TYPE_WPM
  | LLAMA_VOCAB_TYPE_UGM
  | LLAMA_VOCAB_TYPE_RWKV
  deriving (Show, Eq, Ord, Generic)

instance Storable LlamaVocabType where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
      mbVal <- fromLlamaRopeVocabType <$> peek (castPtr ptr)
      case mbVal of
        Nothing -> error "Invalid LlamaVocabType value"
        Just val -> return val
  poke ptr val = do
      let val' = toLlamaRopeVocabType val
      poke (castPtr ptr) val'

toLlamaRopeVocabType :: LlamaVocabType -> CInt
toLlamaRopeVocabType LLAMA_VOCAB_TYPE_NONE = 0
toLlamaRopeVocabType LLAMA_VOCAB_TYPE_SPM  = 1
toLlamaRopeVocabType LLAMA_VOCAB_TYPE_BPE  = 2
toLlamaRopeVocabType LLAMA_VOCAB_TYPE_WPM  = 3
toLlamaRopeVocabType LLAMA_VOCAB_TYPE_UGM  = 4
toLlamaRopeVocabType LLAMA_VOCAB_TYPE_RWKV = 5

fromLlamaRopeVocabType :: CInt -> Maybe LlamaVocabType
fromLlamaRopeVocabType 0 = Just LLAMA_VOCAB_TYPE_NONE
fromLlamaRopeVocabType 1 = Just LLAMA_VOCAB_TYPE_SPM
fromLlamaRopeVocabType 2 = Just LLAMA_VOCAB_TYPE_BPE
fromLlamaRopeVocabType 3 = Just LLAMA_VOCAB_TYPE_WPM
fromLlamaRopeVocabType 4 = Just LLAMA_VOCAB_TYPE_UGM
fromLlamaRopeVocabType 5 = Just LLAMA_VOCAB_TYPE_RWKV
fromLlamaRopeVocabType _ = Nothing
