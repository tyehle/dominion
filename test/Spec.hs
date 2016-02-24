
import Parser
import Data

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "all tests" [parsing]


parsing = testGroup "parser tests"
    [
        testCase "test for the parser" $ parseNotification "(moved somebody (clean))" @?= Update "somebody" (Clean Nothing),

        testCase "test for clean" $ parseFrom play "(clean)" @?= Right (Clean Nothing)
    ]
