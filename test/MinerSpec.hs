module MinerSpec
(
    minerTests
)
where

import Data
import AgentInstances (Miner49er(..))
import Agent

import Test.Tasty
import Test.Tasty.HUnit

agent = Miner49er

fullSuply = [Copper, Silver, Gold,
             Estate, Duchy, Province,
             Mine, Cellar, Market, Remodel, Smithy, Village, Woodcutter, Workshop, Moat, Militia]

mkState actions buys coins cards = GameState ["me", "other"] fullSuply [] actions buys coins [] cards [] []

minerTests = testGroup "miner 49er"
    [
        testGroup "act"
        [
            testCase "mine" $ tryAction agent (mkState 1 1 0 [Mine, Copper]) @?= Left (Act Mine [Copper, Silver]),
            testCase "village" $ tryAction agent (mkState 1 1 0 [Mine, Smithy, Village, Copper]) @?= Left (Act Village []),
            testCase "smithy" $ tryAction agent (mkState 1 1 0 [Mine, Smithy, Copper]) @?= Left (Act Smithy [])
        ],

        testGroup "add"
        [
            testCase "copper" $ tryAdd agent (mkState 0 1 0 [Copper]) @?= Left (Add Copper),
            testCase "silver" $ tryAdd agent (mkState 0 1 0 [Silver]) @?= Left (Add Silver),
            testCase "gold" $ tryAdd agent (mkState 0 1 0 [Gold]) @?= Left (Add Gold),
            testCase "failure" $ tryAdd agent (mkState 0 1 0 []) @?= Right (mkState 0 1 0 [])
        ],

        testGroup "buy"
        [
            testCase "province" $ tryBuy agent (mkState 0 1 8 []) @?= Left (Buy Province),
            testCase "gold" $ tryBuy agent (mkState 0 1 6 []) @?= Left (Buy Gold),
            testCase "mine" $ tryBuy agent (mkState 0 1 5 [Copper]) @?= Left (Buy Mine),
            testCase "militia" $ tryBuy agent (mkState 0 1 4 [Copper]) @?= Left (Buy Militia),
            testCase "militia fail" $ tryBuy agent (mkState 0 1 4 [Militia]) @?= Left (Buy Smithy),
            testCase "smithy" $ tryBuy agent (mkState 0 1 4 [Copper, Militia]) @?= Left (Buy Smithy),
            testCase "village" $ tryBuy agent (mkState 0 1 4 [Smithy, Militia]) @?= Left (Buy Village),
            testCase "silver" $ tryBuy agent (mkState 0 1 4 [Smithy, Village, Militia]) @?= Left (Buy Silver),
            testCase "duchy" $ tryBuy agent (mkState 0 1 5 [Smithy, Village, Mine, Militia]) @?= Left (Buy Duchy)
        ],

        testGroup "defend"
        [
            testCase "reveal moat" $ defend agent (Act Militia []) (mkState 1 1 0 [Estate, Moat, Copper]) @?= Reveal Moat,
            testCase "discard to 3" $ defend agent (Act Militia []) (mkState 1 1 0 [Estate, Copper, Copper, Mine, Silver]) @?= Discard [Estate, Copper]
        ],

        testGroup "discard to"
        [
            testCase "victory cards" $ discardTo agent 2 (mkState 1 1 0 [Estate, Duchy, Province, Copper, Gold]) @?= Discard [Province, Duchy, Estate],
            testCase "other cards" $ discardTo agent 2 (mkState 1 1 0 [Silver, Silver, Copper, Village, Smithy]) @?= Discard [Copper, Village, Silver]
        ]
    ]
