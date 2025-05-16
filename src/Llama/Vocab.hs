module Llama.Vocab where

import Llama.Internal.Foreign
import Llama.Internal.Types
import Foreign
import Foreign.C.String

-- | Get the number of vocab entries
getVocabSize :: Vocab -> IO Int
getVocabSize (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    fromIntegral <$> c_llama_n_vocab (CLlamaVocab vocabPtr)

-- | Get the number of tokens in the vocab
getVocabTokenCount :: Vocab -> IO Int
getVocabTokenCount (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    fromIntegral <$> c_llama_vocab_n_tokens (CLlamaVocab vocabPtr)

-- | Get the text for a token
getVocabTokenText :: Vocab -> LlamaToken -> IO String
getVocabTokenText (Vocab vocabFPtr) token_ = do
  withForeignPtr vocabFPtr $ \vocabPtr -> do
    cText <- c_llama_vocab_get_text (CLlamaVocab vocabPtr) token_
    peekCString cText

-- | Get the score for a token
getVocabTokenScore :: Vocab -> LlamaToken -> IO Float
getVocabTokenScore (Vocab vocabFPtr) token_ = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    realToFrac <$> c_llama_vocab_get_score (CLlamaVocab vocabPtr) token_

-- | Get the attribute for a token
getVocabTokenAttr :: Vocab -> LlamaToken -> IO Int
getVocabTokenAttr (Vocab vocabFPtr) token_ = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    fromIntegral <$> c_llama_vocab_get_attr (CLlamaVocab vocabPtr) token_

-- | Check if a token is end-of-grammar
isVocabTokenEog :: Vocab -> LlamaToken -> IO Bool
isVocabTokenEog (Vocab vocabFPtr) token_ = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    (/= 0) <$> c_llama_vocab_is_eog (CLlamaVocab vocabPtr) token_

-- | Check if a token is a control token
isVocabTokenControl :: Vocab -> LlamaToken -> IO Bool
isVocabTokenControl (Vocab vocabFPtr) token_ = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    (/= 0) <$> c_llama_vocab_is_control (CLlamaVocab vocabPtr) token_

-- | Get the beginning-of-sentence token
getVocabBosToken :: Vocab -> IO LlamaToken
getVocabBosToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_bos (CLlamaVocab vocabPtr)

-- | Get the end-of-sentence token
getVocabEosToken :: Vocab -> IO LlamaToken
getVocabEosToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_eos (CLlamaVocab vocabPtr)

-- | Get the end-of-turn token
getVocabEotToken :: Vocab -> IO LlamaToken
getVocabEotToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_eot (CLlamaVocab vocabPtr)

-- | Get the sentence separator token
getVocabSepToken :: Vocab -> IO LlamaToken
getVocabSepToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_sep (CLlamaVocab vocabPtr)

-- | Get the next-line token
getVocabNlToken :: Vocab -> IO LlamaToken
getVocabNlToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_nl (CLlamaVocab vocabPtr)

-- | Get the padding token
getVocabPadToken :: Vocab -> IO LlamaToken
getVocabPadToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_pad (CLlamaVocab vocabPtr)

-- | Get whether to add BOS token automatically
getVocabAddBOSToken :: Vocab -> IO Bool
getVocabAddBOSToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    (/= 0) <$> c_llama_vocab_get_add_bos (CLlamaVocab vocabPtr)

-- | Get whether to add EOS token automatically
getVocabAddEOSToken :: Vocab -> IO Bool
getVocabAddEOSToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    (/= 0) <$> c_llama_vocab_get_add_eos (CLlamaVocab vocabPtr)

-- | Get the FIM prefix token
getVocabFIMPrefixToken :: Vocab -> IO LlamaToken
getVocabFIMPrefixToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_fim_pre (CLlamaVocab vocabPtr)

-- | Get the FIM suffix token
getVocabFIMSuffixToken :: Vocab -> IO LlamaToken
getVocabFIMSuffixToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_fim_suf (CLlamaVocab vocabPtr)

-- | Get the FIM middle token
getVocabFIMMiddleToken :: Vocab -> IO LlamaToken
getVocabFIMMiddleToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_fim_mid (CLlamaVocab vocabPtr)

-- | Get the FIM pad token
getVocabFIMPADToken :: Vocab -> IO LlamaToken
getVocabFIMPADToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_fim_pad (CLlamaVocab vocabPtr)

-- | Get the FIM separator token
getVocabFIMSeparatorToken :: Vocab -> IO LlamaToken
getVocabFIMSeparatorToken (Vocab vocabFPtr) = do
  withForeignPtr vocabFPtr $ \vocabPtr ->
    c_llama_vocab_fim_sep (CLlamaVocab vocabPtr)