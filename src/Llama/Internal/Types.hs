{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Llama.Internal.Types
  ( -- * Types
    LlamaTokenData (..)
  , LlamaTokenDataArray (..)
  , LlamaBatch (..)
  , AddBos (..)
  , LlamaToken
  , LlamaKvCacheView (..)
  , LlamaSeqId
  , LlamaPos
  , LlamaChatMessage (..)
  , LlamaSamplerI (..)
  , LlamaSampler (..)
  , LlamaSamplerContext 
  , LlamaPerfContextData (..)
  , LlamaPerfSamplerData (..)
  , LlamaLogitBias (..)

    -- * Raw pointers
  , CLlamaVocab (..)
  , CLlamaModel (..)
  , CLlamaContext (..)
  , CLlamaKVCache (..)
  , CLlamaAdapterLora (..)

    -- * Managed handles
  , Vocab (..)
  , Batch (..)
  , Model (..)
  , Context (..)
  , Sampler (..)
  , KVCache (..)
  , AdapterLora (..)
  ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable.Generic
import GHC.Generics

data AddBos = Always | Never

data LlamaLogitBias = LlamaLogitBias
  { tokenLogitBias :: LlamaToken
  , bias  :: Float
  }
  deriving (Show, Eq, Generic, GStorable)

-- struct llama_adapter_lora

newtype CLlamaAdapterLora = CLlamaAdapterLora (Ptr CLlamaAdapterLora)
  deriving (Show, Eq)

-- | Raw pointer to llama_vocab struct
newtype CLlamaVocab = CLlamaVocab (Ptr CLlamaVocab)
  deriving (Show, Eq)

-- | Raw pointer to llama_model struct
newtype CLlamaModel = CLlamaModel (Ptr CLlamaModel)
  deriving (Show, Eq)

-- | Raw pointer to llama_context struct
newtype CLlamaContext = CLlamaContext (Ptr CLlamaContext)
  deriving (Show, Eq)

newtype CLlamaKVCache = CLlamaKVCache (Ptr CLlamaKVCache)
  deriving (Show, Eq)

-- | Managed batch pointer with automatic cleanup
newtype Batch = Batch (Ptr LlamaBatch)
  deriving (Show, Eq)

-- | Managed vocabulary handle with automatic cleanup.
newtype Vocab = Vocab (ForeignPtr CLlamaVocab)
  deriving (Show, Eq)

-- | Managed model handle with automatic cleanup.
newtype Model = Model (ForeignPtr CLlamaModel)
  deriving (Show, Eq)

-- | Managed context handle with automatic cleanup.
newtype Context = Context (ForeignPtr CLlamaContext)
  deriving (Show, Eq)

-- | Managed sampler handle with automatic cleanup.
newtype Sampler = Sampler (ForeignPtr LlamaSampler)
  deriving (Show, Eq)

newtype KVCache = KVCache (ForeignPtr CLlamaKVCache)
  deriving (Show, Eq)

newtype AdapterLora = AdapterLora (ForeignPtr CLlamaAdapterLora)
  deriving (Show, Eq)

-- type PtrLlamaContext = Ptr LlamaContext
type LlamaToken = CInt
type LlamaPos = CInt
type LlamaSeqId = CInt

data LlamaChatMessage = LlamaChatMessage
  { role :: CString
  -- ^ "system", "user", "assistant"
  , content :: CString
  -- ^ Message text
  }
  deriving (Show, Eq, Generic, GStorable)

data LlamaTokenData = LlamaTokenData
  { id :: LlamaToken
  -- ^ token id
  , logit :: CFloat
  -- ^ log-odds of the token
  , p :: CFloat
  -- ^ probability of the token
  }
  deriving (Show, Eq, Generic, GStorable)

data LlamaTokenDataArray = LlamaTokenDataArray
  { data_ :: Ptr LlamaTokenData
  , size_ :: CSize
  , selected :: CLong
  -- ^ this is the index in the data array (i.e. not the token id)
  , sorted :: CBool
  }
  deriving (Eq, Show, Generic, GStorable)

data LlamaBatch = LlamaBatch
  { n_tokens :: CInt
  , token :: Ptr CInt
  , embd :: Ptr CFloat
  , pos :: Ptr CInt
  , n_seq_id :: Ptr CInt
  , seq_id :: Ptr (Ptr CInt)
  , logits :: Ptr CSChar
  -- ^ TODO: rename this to "output"
  }
  deriving (Generic, GStorable, Eq, Show)

-- | Corresponds to `struct llama_kv_cache_view_cell`
newtype LlamaKvCacheViewCell = LlamaKvCacheViewCell
  { posKvCacheViewCell :: LlamaPos
  -- ^ Position in the KV cache (may be negative if unpopulated)
  }
  deriving (Show, Eq, Generic, GStorable)

-- | Corresponds to `struct llama_kv_cache_view`
data LlamaKvCacheView = LlamaKvCacheView
  { n_cells :: CInt
  -- ^ Total number of KV cache cells
  , n_seq_max :: CInt
  -- ^ Max sequences per cell (may not reflect actual)
  , token_count :: CInt
  -- ^ Total tokens in the cache
  , used_cells :: CInt
  -- ^ Number of populated cells
  , max_contiguous :: CInt
  -- ^ Max contiguous empty slots
  , max_contiguous_idx :: CInt
  -- ^ Index to start of max_contiguous (may be -1)
  , cells :: Ptr LlamaKvCacheViewCell
  -- ^ Array of view cells
  , cells_sequences :: Ptr LlamaSeqId
  -- ^ Sequence IDs for each cell (n_seq_max per cell)
  }
  deriving (Show, Eq, Generic, GStorable)

-- | Corresponds to `llama_sampler_context_t` (void *)
type LlamaSamplerContext = Ptr ()

-- | Corresponds to `struct llama_sampler_i`
data LlamaSamplerI = LlamaSamplerI
  { name :: FunPtr (Ptr LlamaSampler -> IO CString)
  -- ^ Optional: const char * (*name)(const struct llama_sampler *)
  , accept :: FunPtr (Ptr LlamaSampler -> LlamaToken -> IO ())
  -- ^ Optional: void (*accept)(struct llama_sampler *, llama_token)
  , apply :: FunPtr (Ptr LlamaSampler -> Ptr LlamaTokenDataArray -> IO ())
  -- ^ Required: void (*apply)(struct llama_sampler *, llama_token_data_array *)
  , reset :: FunPtr (Ptr LlamaSampler -> IO ())
  -- ^ Optional: void (*reset)(struct llama_sampler *)
  , clone :: FunPtr (Ptr LlamaSampler -> IO (Ptr LlamaSampler))
  -- ^ Optional: struct llama_sampler * (*clone)(const struct llama_sampler *)
  , free_ :: FunPtr (Ptr LlamaSampler -> IO ())
  -- ^ Optional: void (*free)(struct llama_sampler *)
  }
  deriving (Show, Eq, Generic, GStorable)

-- | Corresponds to `struct llama_sampler`
data LlamaSampler = LlamaSampler
  { iface :: Ptr LlamaSamplerI
  -- ^ Pointer to interface vtable
  , ctx :: LlamaSamplerContext
  -- ^ Opaque context pointer
  }
  deriving (Generic, Show, Eq, GStorable)

data LlamaPerfContextData = LlamaPerfContextData
    { t_start_ms   :: CDouble  -- ^ Start time in milliseconds
    , t_load_ms    :: CDouble  -- ^ Model load time in milliseconds
    , t_p_eval_ms  :: CDouble  -- ^ Prefill evaluation time in milliseconds
    , t_eval_ms    :: CDouble  -- ^ Generation evaluation time in milliseconds
    , n_p_eval     :: CInt     -- ^ Number of tokens in prefill phase
    , n_eval       :: CInt     -- ^ Number of tokens in generation phase
    }
    deriving (Show, Eq, Generic, GStorable)

data LlamaPerfSamplerData = LlamaPerfSamplerData
    { t_sample_ms  :: CDouble  -- ^ Sampling time in milliseconds
    , n_sample     :: CInt     -- ^ Number of sampling operations
    }
    deriving (Show, Eq, Generic, GStorable)
