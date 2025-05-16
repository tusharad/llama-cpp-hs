module Llama.Split where

import Llama.Internal.Foreign
import Foreign
import Foreign.C.String

-- | Split a path into a prefix and a split number
splitPath :: FilePath -> Int -> Int -> IO (Either String String)
splitPath pathPrefix splitNo splitCount = do
  withCString pathPrefix $ \cPathPrefix -> do
    allocaArray 256 $ \splitPathPtr -> do
      requiredLen <- c_llama_split_path splitPathPtr 256 cPathPrefix (fromIntegral splitNo) (fromIntegral splitCount)
      if requiredLen < 0
        then return $ Left "Failed to split path"
        else do
          str <- peekCString splitPathPtr
          return $ Right str

-- | Get the prefix from a split path
splitPrefix :: FilePath -> Int -> Int -> IO (Either String String)
splitPrefix splitPath_ splitNo splitCount = do
  withCString splitPath_ $ \cSplitPath -> do
    allocaArray 256 $ \splitPrefixPtr -> do
      requiredLen <- c_llama_split_prefix splitPrefixPtr 256 cSplitPath (fromIntegral splitNo) (fromIntegral splitCount)
      if requiredLen < 0
        then return $ Left "Failed to get prefix from split path"
        else do
          str <- peekCString splitPrefixPtr
          return $ Right str

-- | Print system information
printSystemInfo :: IO String
printSystemInfo = do
  systemInfo <- c_llama_print_system_info
  peekCString systemInfo