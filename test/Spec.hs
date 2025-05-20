import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit
import qualified Llama.Backend as Backend
import Control.Exception (bracket)
import qualified Llama.ChatTemplate as ChatTemplate
import Data.Either

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
     testBackend
    , testChatApplyTemplate
    ]

testBackend :: TestTree
testBackend = testGroup "Llama.Backend tests" [
    testCase "llamaBackendInit and llamaBackendFree should not throw exceptions" $ do
        Backend.llamaBackendInit
        Backend.llamaBackendFree
        return ()

    , testCase "llamaBackendInit and llamaBackendFree should be idempotent" $ do
        Backend.llamaBackendInit
        Backend.llamaBackendFree
        Backend.llamaBackendFree
        return ()

    , testCase "llamaBackendInit can be called multiple times" $ do
        Backend.llamaBackendInit
        Backend.llamaBackendInit
        Backend.llamaBackendFree
        Backend.llamaBackendFree
        return ()

    , testCase "llamaBackendInit and llamaBackendFree should not throw exceptions" $ do
            bracket Backend.llamaBackendInit (const Backend.llamaBackendFree) $ \_ -> return ()
    ]

testChatApplyTemplate :: TestTree
testChatApplyTemplate = testGroup "chatApplyTemplate tests" [
      testCase "chatApplyTemplate with no template" $ do
        messages <- ChatTemplate.chatApplyTemplate Nothing [ChatTemplate.ChatMessage "role" "content"] False 4096
        assertBool "result is not Left" (isRight messages),
      -- testCase "chatApplyTemplate with custom template" $ do
      --   messages <- ChatTemplate.chatApplyTemplate (Just "template") [ChatTemplate.ChatMessage "role" "content"] False 4096
      --   assertBool "result is not Left" (isRight messages), -- throwing error as of now
      testCase "chatApplyTemplate with empty messages" $ do
        messages <- ChatTemplate.chatApplyTemplate Nothing [] False 4096
        assertBool "result is not Left" (isRight messages)
      ]
