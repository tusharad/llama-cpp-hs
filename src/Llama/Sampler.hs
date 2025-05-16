module Llama.Sampler where

import Llama.Internal.Types
import Llama.Internal.Types.Params
import Foreign
import Llama.Internal.Foreign
import Foreign.C.String

-- | Convert Storable Haskell struct to pointer and run an action
withStorable :: Storable a => a -> (Ptr a -> IO b) -> IO b
withStorable x f = alloca $ \ptr -> do
  poke ptr x
  f ptr

-- | Get the default parameters for a sampler chain
defaultSamplerChainParams :: IO LlamaSamplerChainParams
defaultSamplerChainParams = alloca $ \paramsPtr -> do
      c_llama_sampler_chain_default_params_into paramsPtr
      peek paramsPtr

-- | Initialize a sampler
initSampler :: LlamaSamplerI -> LlamaSamplerContext -> IO (Either String Sampler)
initSampler iface_ ctx_ = do
  ifacePtr <- mallocBytes (sizeOf (undefined :: LlamaSamplerI))
  poke ifacePtr iface_
  samplerPtr <- c_llama_sampler_init ifacePtr ctx_
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right (Sampler fp)

-- | Get the name of a sampler
getSamplerName :: Sampler -> IO String
getSamplerName (Sampler samplerFPtr) = do
  withForeignPtr samplerFPtr $ \samplerPtr -> do
    name_ <- c_llama_sampler_name samplerPtr
    peekCString name_

-- | Accept a token with a sampler
acceptTokenWithSampler :: Sampler -> LlamaToken -> IO ()
acceptTokenWithSampler (Sampler samplerFPtr) token_ = do
  withForeignPtr samplerFPtr $ \samplerPtr ->
    c_llama_sampler_accept samplerPtr token_

-- | Apply a sampler to a token data array
applySampler :: Sampler -> LlamaTokenDataArray -> IO ()
applySampler (Sampler samplerFPtr) tokenDataArray = do
  withForeignPtr samplerFPtr $ \samplerPtr ->
    withStorable tokenDataArray $ \tokenDataArrayPtr ->
      c_llama_sampler_apply samplerPtr tokenDataArrayPtr

-- | Reset a sampler
resetSampler :: Sampler -> IO ()
resetSampler (Sampler samplerFPtr) = do
  withForeignPtr samplerFPtr $ \samplerPtr ->
    c_llama_sampler_reset (castPtr samplerPtr)

-- | Clone a sampler
cloneSampler :: Sampler -> IO (Either String Sampler)
cloneSampler (Sampler samplerFPtr) = do
  withForeignPtr samplerFPtr $ \samplerPtr -> do
    clonedSamplerPtr <- c_llama_sampler_clone (castPtr samplerPtr)
    if clonedSamplerPtr == nullPtr
      then return $ Left "Failed to clone sampler"
      else do
        fp <- newForeignPtr p_llama_sampler_free clonedSamplerPtr
        return $ Right $ Sampler fp

-- | Initialize a sampler chain
initSamplerChain :: LlamaSamplerChainParams -> IO (Either String Sampler)
initSamplerChain params = do
  chainPtr <- c_llama_sampler_chain_init params
  if chainPtr == nullPtr
    then return $ Left "Failed to initialize sampler chain"
    else do
      fp <- newForeignPtr p_llama_sampler_free chainPtr
      return $ Right $ Sampler fp

-- | Add a sampler to a sampler chain
addSamplerToChain :: Sampler -> Ptr LlamaSampler -> IO ()
addSamplerToChain (Sampler chainFPtr) samplerPtr = do
  withForeignPtr chainFPtr $ \chainPtr ->
      c_llama_sampler_chain_add chainPtr samplerPtr

getSamplerFromChain :: Sampler -> Int -> IO (Either String Sampler)
getSamplerFromChain (Sampler chainFPtr) index = do
  withForeignPtr chainFPtr $ \chainPtr -> do
    samplerPtr <- c_llama_sampler_chain_get chainPtr (fromIntegral index)
    if samplerPtr == nullPtr
      then return $ Left "Failed to get sampler from chain"
      else do
        fp <- newForeignPtr p_llama_sampler_free samplerPtr
        return $ Right $ Sampler fp

-- | Get the number of samplers in a sampler chain
getSamplerChainLength :: Sampler -> IO Int
getSamplerChainLength (Sampler chainFPtr) = do
  withForeignPtr chainFPtr $ \chainPtr -> do
    fromIntegral <$> c_llama_sampler_chain_n chainPtr

-- | Remove a sampler from a sampler chain
removeSamplerFromChain :: Sampler -> Int -> IO (Either String Sampler)
removeSamplerFromChain (Sampler chainFPtr) index = do
  withForeignPtr chainFPtr $ \chainPtr -> do
    samplerPtr <- c_llama_sampler_chain_remove chainPtr (fromIntegral index)
    if samplerPtr == nullPtr
      then return $ Left "Failed to remove sampler from chain"
      else do
        fp <- newForeignPtr p_llama_sampler_free samplerPtr
        return $ Right $ Sampler fp

-- | Initialize a greedy sampler
initGreedySampler :: IO (Either String (Ptr LlamaSampler))
initGreedySampler = do
  samplerPtr <- c_llama_sampler_init_greedy
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize greedy sampler"
    else do
      return $ Right samplerPtr

-- | Initialize a distributed sampler
initDistributedSampler :: Word32 -> IO (Either String Sampler)
initDistributedSampler seed = do
  samplerPtr <- c_llama_sampler_init_dist (fromIntegral seed)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize distributed sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a top-k sampler
initTopKSampler :: Int -> IO (Either String Sampler)
initTopKSampler k = do
  samplerPtr <- c_llama_sampler_init_top_k (fromIntegral k)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize top-k sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a top-p sampler
initTopPSampler :: Float -> Int -> IO (Either String Sampler)
initTopPSampler p1 minKeep = do
  samplerPtr <- c_llama_sampler_init_top_p (realToFrac p1) (fromIntegral minKeep)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize top-p sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a min-p sampler
initMinPSampler :: Float -> Int -> IO (Either String Sampler)
initMinPSampler p1 minKeep = do
  samplerPtr <- c_llama_sampler_init_min_p (realToFrac p1) (fromIntegral minKeep)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize min-p sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a typical sampler
initTypicalSampler :: Float -> Int -> IO (Either String Sampler)
initTypicalSampler p_ minKeep = do
  samplerPtr <- c_llama_sampler_init_typical (realToFrac p_) (fromIntegral minKeep)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize typical sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a temperature sampler
initTempSampler :: Float -> IO (Either String Sampler)
initTempSampler t = do
  samplerPtr <- c_llama_sampler_init_temp (realToFrac t)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize temperature sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize an extended temperature sampler
initTempExtSampler :: Float -> Float -> Float -> IO (Either String Sampler)
initTempExtSampler t delta exponent_ = do
  samplerPtr <- c_llama_sampler_init_temp_ext (realToFrac t) (realToFrac delta) (realToFrac exponent_)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize extended temperature sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize an XTC sampler
initXTCSampler :: Float -> Float -> Int -> Word32 -> IO (Either String Sampler)
initXTCSampler p1 t minKeep seed = do
  samplerPtr <- c_llama_sampler_init_xtc (realToFrac p1) (realToFrac t) (fromIntegral minKeep) (fromIntegral seed)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize XTC sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a top-N sigma sampler
initTopNSigmaSampler :: Float -> IO (Either String Sampler)
initTopNSigmaSampler n = do
  samplerPtr <- c_llama_sampler_init_top_n_sigma (realToFrac n)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize top-N sigma sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a Mirostat sampler
initMirostatSampler :: Int -> Word32 -> Float -> Float -> Int -> IO (Either String Sampler)
initMirostatSampler nVocab seed tau eta m = do
  samplerPtr <- c_llama_sampler_init_mirostat (fromIntegral nVocab) (fromIntegral seed) (realToFrac tau) (realToFrac eta) (fromIntegral m)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize Mirostat sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a Mirostat V2 sampler
initMirostatV2Sampler :: Word32 -> Float -> Float -> IO (Either String Sampler)
initMirostatV2Sampler seed tau eta = do
  samplerPtr <- c_llama_sampler_init_mirostat_v2 (fromIntegral seed) (realToFrac tau) (realToFrac eta)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize Mirostat V2 sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

-- | Initialize a grammar sampler
initGrammarSampler :: Vocab -> String -> String -> IO (Either String Sampler)
initGrammarSampler (Vocab vocab) grammarStr grammarRoot = do
  withForeignPtr vocab $ \vocabPtr -> do
    withCString grammarStr $ \grammarStrPtr -> do
      withCString grammarRoot $ \grammarRootPtr -> do
        samplerPtr <- c_llama_sampler_init_grammar (CLlamaVocab vocabPtr) grammarStrPtr grammarRootPtr
        if samplerPtr == nullPtr
          then return $ Left "Failed to initialize grammar sampler"
          else do
            fp <- newForeignPtr p_llama_sampler_free samplerPtr
            return $ Right $ Sampler fp

-- | Initialize a grammar sampler with lazy patterns
initGrammarLazyPatternsSampler :: Vocab -> String -> String -> [String] -> [LlamaToken] -> IO (Either String Sampler)
initGrammarLazyPatternsSampler (Vocab vocab) grammarStr grammarRoot triggerPatterns triggerTokens = do
  withForeignPtr vocab $ \vocabPtr -> do
    withCString grammarStr $ \grammarStrPtr -> do
      withCString grammarRoot $ \grammarRootPtr -> do
        triggerPatternsPtr <- newArrayOfPtrs triggerPatterns
        withForeignPtr triggerPatternsPtr $ \triggerPatternsPtr' -> do
          triggerTokensPtr <- mallocBytes (length triggerTokens * sizeOf (undefined :: LlamaToken))
          pokeArray triggerTokensPtr triggerTokens
          samplerPtr <- c_llama_sampler_init_grammar_lazy_patterns
            (CLlamaVocab vocabPtr)
            grammarStrPtr
            grammarRootPtr
            triggerPatternsPtr'
            (fromIntegral (length triggerPatterns))
            triggerTokensPtr
            (fromIntegral (length triggerTokens))
          if samplerPtr == nullPtr
            then return $ Left "Failed to initialize grammar lazy patterns sampler"
            else do
              fp <- newForeignPtr p_llama_sampler_free samplerPtr
              return $ Right $ Sampler fp

-- | Initialize a penalties sampler
initPenaltiesSampler :: Int -> Float -> Float -> Float -> IO (Either String Sampler)
initPenaltiesSampler penaltyLastN penaltyRepeat penaltyFreq penaltyPresent = do
  samplerPtr <- c_llama_sampler_init_penalties
    (fromIntegral penaltyLastN)
    (realToFrac penaltyRepeat)
    (realToFrac penaltyFreq)
    (realToFrac penaltyPresent)
  if samplerPtr == nullPtr
    then return $ Left "Failed to initialize penalties sampler"
    else do
      fp <- newForeignPtr p_llama_sampler_free samplerPtr
      return $ Right $ Sampler fp

newArrayOfPtrs :: [String] -> IO (ForeignPtr CString)
newArrayOfPtrs xs = do
  ptrs <- mallocBytes (length xs * sizeOf (undefined :: Ptr CString))
  mapM_ (\(i, x) -> withCString x $ \cstr -> poke (castPtr ptrs `plusPtr` (i * sizeOf (undefined :: Ptr CString))) cstr) (zip [0..] xs)
  newForeignPtr_ ptrs

-- Helper function
withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray xs f = do
  ptrs <- newArrayOfPtrs xs
  withForeignPtr ptrs $ \ptrs' ->
    f ptrs'

-- | Initialize a dry sampler
initDrySampler :: Vocab -> Int -> Float -> Float -> Int -> Int -> [String] -> IO (Either String Sampler)
initDrySampler (Vocab vocab) nCtxTrain dryMultiplier dryBase dryAllowedLength dryPenaltyLastN seqBreakers = do
  withForeignPtr vocab $ \vocabPtr -> do
    withCStringArray seqBreakers $ \seqBreakersPtr -> do
      samplerPtr <- c_llama_sampler_init_dry
        (CLlamaVocab vocabPtr)
        (fromIntegral nCtxTrain)
        (realToFrac dryMultiplier)
        (realToFrac dryBase)
        (fromIntegral dryAllowedLength)
        (fromIntegral dryPenaltyLastN)
        seqBreakersPtr
        (fromIntegral (length seqBreakers))
      if samplerPtr == nullPtr
        then return $ Left "Failed to initialize dry sampler"
        else do
          fp <- newForeignPtr p_llama_sampler_free samplerPtr
          return $ Right $ Sampler fp

-- | Initialize a logit bias sampler
initLogitBiasSampler :: Int -> [LlamaLogitBias] -> IO (Either String Sampler)
initLogitBiasSampler nVocab logitBiases = do
  allocaArray (length logitBiases) $ \logitBiasPtr -> do
    pokeArray logitBiasPtr logitBiases
    samplerPtr <- c_llama_sampler_init_logit_bias
      (fromIntegral nVocab)
      (fromIntegral (length logitBiases))
      logitBiasPtr
    if samplerPtr == nullPtr
      then return $ Left "Failed to initialize logit bias sampler"
      else do
        fp <- newForeignPtr p_llama_sampler_free samplerPtr
        return $ Right $ Sampler fp

-- | Initialize an infill sampler
initInfillSampler :: Vocab -> IO (Either String Sampler)
initInfillSampler (Vocab vocab) = do
  withForeignPtr vocab $ \vocabPtr -> do
    samplerPtr <- c_llama_sampler_init_infill (CLlamaVocab vocabPtr)
    if samplerPtr == nullPtr
      then return $ Left "Failed to initialize infill sampler"
      else do
        fp <- newForeignPtr p_llama_sampler_free samplerPtr
        return $ Right $ Sampler fp

-- | Get the seed used by a sampler
getSamplerSeed :: Sampler -> IO Word32
getSamplerSeed (Sampler samplerFPtr) = do
  withForeignPtr samplerFPtr (fmap fromIntegral . c_llama_sampler_get_seed)

-- | Sample with a sampler
sampleWithSampler :: Sampler -> Context -> Int -> IO LlamaToken
sampleWithSampler (Sampler samplerFPtr) (Context ctxFPtr) idx = do
  withForeignPtr samplerFPtr $ \samplerPtr -> do
    withForeignPtr ctxFPtr $ \ctxPtr ->
      c_llama_sampler_sample samplerPtr (CLlamaContext ctxPtr) (fromIntegral idx)
