{- |
Module      : Llama.State
Description : High level State interface for llama-cpp
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-}
module Llama.State (
   getStateSize
, getStateData
, setStateData
, loadStateFromFile
, saveStateToFile
, getSequenceStateSize
, setSequenceStateData
, saveSequenceStateToFile
, loadSequenceStateFromFile
) where

import Llama.Internal.Foreign
import Llama.Internal.Types
import Foreign
import Foreign.C.String
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

-- | Get the size of the state
getStateSize :: Context -> IO Word64
getStateSize (Context ctxFPtr) = do
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> c_llama_state_get_size (CLlamaContext ctxPtr)

-- | Get the state data
getStateData :: Context -> IO ByteString
getStateData (Context ctxFPtr) = do
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    size <- c_llama_state_get_size (CLlamaContext ctxPtr)
    allocaBytes (fromIntegral size) $ \dstPtr -> do
      _ <- c_llama_state_get_data (CLlamaContext ctxPtr) dstPtr size
      BS.pack <$> peekArray (fromIntegral size) dstPtr

-- | Set the state data
setStateData :: Context -> ByteString -> IO ()
setStateData (Context ctxFPtr) bs = do
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    withArray (BS.unpack bs) $ \srcPtr -> do
      _ <- c_llama_state_set_data (CLlamaContext ctxPtr) srcPtr (fromIntegral (BS.length bs))
      return ()

-- | Load a state from a file
loadStateFromFile :: Context -> FilePath -> [LlamaToken] -> IO [LlamaToken]
loadStateFromFile (Context ctxFPtr) pathSession tokens = do
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    withCString pathSession $ \cPathSession -> do
      allocaArray (length tokens) $ \tokensOutPtr -> do
        alloca $ \nTokenCountOutPtr -> do
          success <- c_llama_state_load_file
            (CLlamaContext ctxPtr)
            cPathSession
            tokensOutPtr
            (fromIntegral (length tokens))
            nTokenCountOutPtr
          if success == 0
            then return []
            else do
            tokenCount <- peek nTokenCountOutPtr
            peekArray (fromIntegral tokenCount) tokensOutPtr

-- | Save a state to a file
saveStateToFile :: Context -> FilePath -> [LlamaToken] -> IO Bool
saveStateToFile (Context ctxFPtr) pathSession tokens = do
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    withCString pathSession $ \cPathSession -> do
      withArray tokens $ \tokensPtr -> do
        toBool <$> c_llama_state_save_file
          (CLlamaContext ctxPtr)
          cPathSession
          tokensPtr
          (fromIntegral (length tokens))

-- | Get the size of a sequence in the state
getSequenceStateSize :: Context -> LlamaSeqId -> IO Word64
getSequenceStateSize (Context ctxFPtr) seqId = do
  withForeignPtr ctxFPtr $ \ctxPtr ->
    fromIntegral <$> c_llama_state_seq_get_size (CLlamaContext ctxPtr) seqId

-- | Set the state data for a sequence
setSequenceStateData :: Context -> ByteString -> LlamaSeqId -> IO Word64
setSequenceStateData (Context ctxFPtr) bs seqId = do
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    withArray (BS.unpack bs) $ \srcPtr -> do
      fromIntegral <$>
        c_llama_state_seq_set_data (CLlamaContext ctxPtr) srcPtr (fromIntegral (BS.length bs)) seqId

-- | Save a sequence state to a file
saveSequenceStateToFile :: Context -> FilePath -> LlamaSeqId -> [LlamaToken] -> IO Word64
saveSequenceStateToFile (Context ctxFPtr) filepath seqId tokens = do
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    withArray tokens $ \tokenPtr -> do
      withCString filepath $ \cfilepath -> do
        fromIntegral <$>
          c_llama_state_seq_save_file (CLlamaContext ctxPtr) cfilepath seqId tokenPtr (fromIntegral (length tokens))

-- | Load a sequence state from a file
loadSequenceStateFromFile :: Context -> FilePath -> LlamaSeqId -> [LlamaToken] -> IO [LlamaToken]
loadSequenceStateFromFile (Context ctxFPtr) filepath destSeqId tokens = do
  withForeignPtr ctxFPtr $ \ctxPtr -> do
    withCString filepath $ \cfilepath -> do
      allocaArray (length tokens) $ \tokensOutPtr -> do
        alloca $ \nTokenCountOutPtr -> do
          _ <- c_llama_state_seq_load_file
            (CLlamaContext ctxPtr)
            cfilepath
            destSeqId
            tokensOutPtr
            (fromIntegral (length tokens))
            nTokenCountOutPtr
          tokenCount <- peek nTokenCountOutPtr
          peekArray (fromIntegral tokenCount) tokensOutPtr
