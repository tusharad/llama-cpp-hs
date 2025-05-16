module Llama.Backend where
{-
Backend and intialization related functions for Llama.cpp
-}

import Llama.Internal.Foreign.Backend

llamaBackendInit :: IO ()
llamaBackendInit = c_llama_backend_init

llamaBackendFree :: IO ()
llamaBackendFree = c_llama_backend_free
