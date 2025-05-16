module Main (main) where

import Foreign
import Data.Either

import Llama.Internal.Types
import Llama.Internal.Types.Params
import Llama.Internal.Foreign
import Llama.Model
import Llama.Context
import Llama.Backend
import Llama.Sampler
import Llama.Decode
import Llama.Vocab
import Llama.Tokenize

main :: IO ()
main = do
    let modelPath = "path/to/model.gguf"
        prompt = "Hey, how are you?"
        n_predict = 100 :: Int
    llamaBackendInit
    modelParams_ <- defaultModelParams 
    let modelParams = modelParams_ {
        nGpuLayers = 99
    }
    eModel <- loadModelFromFile modelPath modelParams
    case eModel of
      Left err -> putStrLn err
      Right model -> do 
        eVocab <- getModelVocab model
        case eVocab of
          Left err -> putStrLn err
          Right vocab -> do 
            (tokenList, tokenCount) <- tokenize vocab prompt True True
            contextParams_ <- defaultContextParams 
            let ctxParams = contextParams_ {
                n_ctx = fromIntegral $ tokenCount + n_predict - 1
              , n_batch = fromIntegral tokenCount
              , no_perf = fromBool False
            }
            eCtx <- initContextFromModel model ctxParams
            case eCtx of
              Left err -> putStrLn err
              Right ctx -> do 
                samplerChainParams_ <- defaultSamplerChainParams 
                let samplerChainParams = samplerChainParams_ {
                    noPerf = fromBool False
                }
                eSampler <- initSamplerChain samplerChainParams
                case eSampler of
                  Left err -> putStrLn err
                  Right sampler -> do 
                    eGreedy <- initGreedySampler 
                    case eGreedy of 
                      Left err -> putStrLn err
                      Right greedySampelr -> do 
                        addSamplerToChain sampler greedySampelr
                        mapM_ (\t -> tokenToPiece vocab t True >>= putStr) tokenList
                        batch@(Batch batchPtr) <- batchGetOne tokenList
                        llamaBatch <- peek batchPtr
                        let nTokens = n_tokens llamaBatch
                        go 0 tokenCount 
                            n_predict 
                            ctx 
                            batch sampler vocab (fromIntegral nTokens)
                        putStrLn ""
                    
    llamaBackendFree
  where 
    go :: 
        Int 
        -> Int 
        -> Int 
        -> Context 
        -> Batch -> Sampler -> Vocab -> Int -> IO ()
    go n_pos 
        tokenCount 
        n_predict 
        ctx batch@(Batch batchPtr) sampler vocab nT = do 
      if n_pos + nT >= tokenCount + n_predict then 
        pure ()
      else do 
        eRes <- decodeBatch ctx batch
        case eRes of
         Left err -> putStrLn err
         Right _ -> do
           llamaBatch <- peek batchPtr
           let nTokens = n_tokens llamaBatch
           newTokenId <- sampleWithSampler sampler ctx (-1)
           b <- isVocabTokenEog vocab newTokenId
           if b then 
             pure ()
           else do 
             str <- tokenToPiece vocab newTokenId True
             putStr str
             newBatch <- batchGetOne [newTokenId]
             go (n_pos + fromIntegral nTokens) 
                tokenCount 
                n_predict 
                ctx newBatch sampler vocab (fromIntegral nTokens)
