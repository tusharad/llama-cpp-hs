module Llama.ChatTemplate where

import Data.Functor
import Foreign
import Foreign.C.String
import Llama.Internal.Foreign
import Llama.Internal.Types

data ChatMessage = ChatMessage
  { chatRole :: String
  , chatContent :: String
  }
  deriving (Show, Eq)

toCLlamaChatMessage :: ChatMessage -> IO LlamaChatMessage
toCLlamaChatMessage msg = do
  rolePtr <- newCString (chatRole msg)
  contentPtr <- newCString (chatContent msg)
  return $ LlamaChatMessage rolePtr contentPtr

-- | Apply a chat template to format a conversation.
chatApplyTemplate ::
  -- | Optional custom template (uses built-in if Nothing)
  Maybe String ->
  -- | List of chat messages
  [ChatMessage] ->
  -- | Add assistant token at end?
  Bool ->
  -- | Buffer size (suggested: 4096)
  Int ->
  -- | Returns formatted string or error message
  IO (Either String String)
chatApplyTemplate mTemplate messages addAssist bufferSize = do
  let nMessages = length messages
      bufSize = max bufferSize (nMessages * 64) -- Heuristic fallback
  templateCString <- maybe (return nullPtr) newCString mTemplate
  msgs <- mapM toCLlamaChatMessage messages
  cMessages <- withArray msgs $ \ptr ->
    return ptr

  allocaBytes bufSize $ \bufPtr -> do
    result <-
      c_llama_chat_apply_template
        templateCString
        cMessages
        (fromIntegral nMessages)
        (if addAssist then 1 else 0)
        bufPtr
        (fromIntegral bufSize)

    if result < 0
      then return $ Left "Failed to apply chat template"
      else do
        let actualSize = fromIntegral result
        if actualSize >= bufSize
          then -- Need larger buffer
            allocaBytes actualSize $ \newBufPtr -> do
              result' <-
                c_llama_chat_apply_template
                  templateCString
                  cMessages
                  (fromIntegral nMessages)
                  (if addAssist then 1 else 0)
                  newBufPtr
                  (fromIntegral actualSize)
              if result' < 0
                then return $ Left "Failed to apply chat template after resize"
                else peekCString newBufPtr <&> Right
          else do
            str <- peekCString bufPtr
            return $ Right str

-- | Get list of available built-in chat templates.
chatGetBuiltinTemplates :: IO [String]
chatGetBuiltinTemplates = do
  -- First get number of templates
  numTemplates <- c_llama_chat_builtin_templates nullPtr 0
  let maxTemplates = fromIntegral numTemplates

  if numTemplates <= 0
    then return []
    else do
      -- Allocate array of CString pointers
      arrPtr <- mallocArray maxTemplates
      result <- c_llama_chat_builtin_templates arrPtr (fromIntegral maxTemplates)
      if result < 0
        then return []
        else do
          cstrs <- peekArray maxTemplates arrPtr
          strs <- mapM peekCString cstrs
          free arrPtr
          return strs
