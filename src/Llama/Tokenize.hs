{- |
Module      : Llama.Tokenize
Description : High level Tokenize interface for llama-cpp
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-}
module Llama.Tokenize (
    tokenize
  ,  tokenToPiece
  ,  detokenize
) where

import Llama.Internal.Foreign
import Llama.Internal.Types
import Foreign
import Foreign.C.String

-- | Tokenize a string into tokens
tokenize :: Vocab -> String -> Bool -> Bool -> IO ([LlamaToken], Int)
tokenize (Vocab vocab) text addSpecial parseSpecial = do
  withForeignPtr vocab $ \vocabPtr -> do
    withCString text $ \cText -> do
      let textLen = length text
      allocaArray textLen $ \tokensPtr -> do
        tokenCount <- c_llama_tokenize
          (CLlamaVocab vocabPtr)
          cText
          (fromIntegral textLen)
          tokensPtr
          (fromIntegral textLen)
          (fromBool addSpecial)
          (fromBool parseSpecial)
        tokens <- peekArray (fromIntegral tokenCount) tokensPtr
        return (tokens, fromIntegral tokenCount)

-- | Convert a token to a piece of text
tokenToPiece :: Vocab -> LlamaToken -> Bool -> IO String
tokenToPiece (Vocab vocab) token_ special = do
  withForeignPtr vocab $ \vocabPtr -> do
    allocaArray 256 $ \bufPtr -> do
      _ <- c_llama_token_to_piece
        (CLlamaVocab vocabPtr)
        token_
        bufPtr
        256
        0
        (fromBool special)
      peekCString bufPtr

-- | Detokenize tokens into a string
detokenize :: Vocab -> [LlamaToken] -> Bool -> Bool -> IO String
detokenize (Vocab vocab) tokens removeSpecial unparseSpecial = do
  withForeignPtr vocab $ \vocabPtr -> do
    allocaArray 256 $ \textPtr -> do
      withArray tokens $ \tokensPtr -> do
        _ <- c_llama_detokenize
            (CLlamaVocab vocabPtr)
            tokensPtr
            (fromIntegral $ length tokens)
            textPtr
            256
            (fromBool removeSpecial)
            (fromBool unparseSpecial)
        peekCString textPtr
