{- |
Module      : Llama.Backend
Description : Backend related functions for llama-cpp
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-}
module Llama.Backend (
    llamaBackendInit
    , llamaBackendFree
) where
{-
Backend and intialization related functions for Llama.cpp
-}

import Llama.Internal.Foreign.Backend

llamaBackendInit :: IO ()
llamaBackendInit = c_llama_backend_init

llamaBackendFree :: IO ()
llamaBackendFree = c_llama_backend_free
