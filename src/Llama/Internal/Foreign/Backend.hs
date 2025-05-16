module Llama.Internal.Foreign.Backend
  ( -- * Backend
    c_llama_backend_init
  , c_llama_backend_free
  ) where

-- | Initialize the llama backend
foreign import ccall "_llama_backend_init" c_llama_backend_init :: IO ()

-- | Free the llama backend resources
foreign import ccall "_llama_backend_free" c_llama_backend_free :: IO ()
