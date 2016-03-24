module SmithySpec
(
    smithyTests
)
where

import Data
import AgentInstances (SmithyMoney(..))
import Agent

import Data.List (replicate)
import Test.Tasty
import Test.Tasty.HUnit

agent = SmithyMoney

fullSupply = replicate 60 Copper ++ replicate 40 Silver ++ replicate 30 Gold
            ++ (concat . map (replicate 8) $ [Estate, Duchy, Province])
            ++ (concat . map (replicate 10) $ [Mine, Cellar, Market, Remodel, Smithy, Village, Woodcutter, Workshop, Moat, Militia])

endGameSupply = [Copper, Silver, Gold,
                 Estate, Duchy, Province,
                 Mine, Cellar, Market, Remodel, Smithy, Village, Workshop, Woodcutter, Moat, Militia]

mkState supply actions buys coins cards = GameState ["me", "other"] supply [] actions buys coins [Estate] cards [] [Gold, Silver, Copper]

smithyTests = testGroup "money smithy"
    [
        testGroup "act"
        [
            testCase "smithy" $ tryAction agent (mkState fullSupply 1 1 0 [Silver, Smithy, Copper]) @?= Left (Act Smithy []),
            testCase "failure" $ tryAction agent (mkState fullSupply 1 1 0 [Mine, Silver, Militia]) @?= Right (mkState fullSupply 1 1 0 [Mine, Silver, Militia])
        ],

        testGroup "add"
        [
            testCase "copper" $ tryAdd agent (mkState fullSupply 0 1 0 [Copper]) @?= Left (Add Copper),
            testCase "silver" $ tryAdd agent (mkState fullSupply 0 1 0 [Silver]) @?= Left (Add Silver),
            testCase "gold" $ tryAdd agent (mkState fullSupply 0 1 0 [Gold]) @?= Left (Add Gold),
            testCase "failure" $ tryAdd agent (mkState fullSupply 0 1 0 []) @?= Right (mkState fullSupply 0 1 0 [])
        ],

        testGroup "buy"
        [
            testCase "province" $ tryBuy agent (mkState fullSupply 0 1 8 []) @?= Left (Buy Province),
            testCase "gold" $ tryBuy agent (mkState fullSupply 0 1 6 [Smithy]) @?= Left (Buy Gold),
            testCase "gold fail" $ tryBuy agent (mkState endGameSupply 0 1 6 [Smithy]) @?= Left (Buy Duchy),
            testCase "smithy" $ tryBuy agent (mkState fullSupply 0 1 4 [Copper]) @?= Left (Buy Smithy),
            testCase "silver" $ tryBuy agent (mkState fullSupply 0 1 4 [Smithy]) @?= Left (Buy Silver),
            testCase "duchy" $ tryBuy agent (mkState endGameSupply 0 1 5 [Smithy]) @?= Left (Buy Duchy)
        ],

        testGroup "defend"
        [
            testCase "reveal moat" $ defend agent (Act Militia []) (mkState fullSupply 1 1 0 [Estate, Moat, Copper]) @?= Reveal Moat,
            testCase "discard to 3" $ defend agent (Act Militia []) (mkState fullSupply 1 1 0 [Estate, Copper, Copper, Mine, Silver]) @?= Discard [Estate, Copper]
        ],

        testGroup "discard to"
        [
            testCase "victory cards" $ discardTo agent (mkState fullSupply 1 1 0 [Estate, Duchy, Province, Copper, Gold]) 2 @?= Discard [Province, Duchy, Estate],
            testCase "other cards" $ discardTo agent (mkState fullSupply 1 1 0 [Silver, Silver, Copper, Smithy]) 2 @?= Discard [Copper, Silver]
        ]
    ]
