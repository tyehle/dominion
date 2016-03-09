
import Parser
import Data
import Agent

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "all tests" [parsing, agent, dataTests]



parsing = testGroup "parser tests"
    [
        testGroup "labeledList"
        [
            testCase "empty list" $ parseFrom (labeledList "test" number) "(test)" @?= Right [],
            testCase "empty spaces" $ parseFrom (labeledList "test" number) "(  test  )" @?= Right [],
            testCase "one element" $ parseFrom (labeledList "test" number) "(test 0)" @?= Right [0],
            testCase "one spaces" $ parseFrom (labeledList "test" number) "( test 0 )" @?= Right [0],
            testCase "two spaces" $ parseFrom (labeledList "test" number) "( test 0   1   )" @?= Right [0, 1]
        ],

        testGroup "play"
        [
            testCase "clean nothing" $ parseFrom play "(clean)" @?= Right (Clean Nothing),
            testCase "clean copper" $ parseFrom play "(clean copper)" @?= Right (Clean (Just Copper)),
            testCase "clean with space" $ parseFrom play "( clean  estate )"  @?= Right (Clean (Just Estate)),

            testCase "buy" $ parseFrom play "(buy mine)" @?= Right (Buy Mine),

            testCase "add" $ parseFrom play "(add copper)" @?= Right (Add Copper),

            testGroup "act"
            [
                testCase "mine" $ parseFrom play "(act mine copper silver)" @?= Right (Act Mine [Copper, Silver]),
                testCase "cellar" $ parseFrom play "(act cellar province mine duchy copper)" @?= Right (Act Cellar [Province, Mine, Duchy, Copper]),
                testCase "market" $ parseFrom play "(act market)" @?= Right (Act Market []),
                testCase "remodel" $ parseFrom play "(act remodel gold province)" @?= Right (Act Remodel [Gold, Province]),
                testCase "smithy" $ parseFrom play "(act smithy)" @?= Right (Act Smithy []),
                testCase "village" $ parseFrom play "(act village)" @?= Right (Act Village []),
                testCase "woodcutter" $ parseFrom play "(act woodcutter)" @?= Right (Act Woodcutter []),
                testCase "workshop" $ parseFrom play "(act workshop village)" @?= Right (Act Workshop [Village])
            ]
        ],

        testGroup "state"
        [
            testCase "with empties" $ parseFrom state "((players) (supply) (trash) (actions 0) (buys 0) (coins 0) (deck) (hand) (plays) (discards))"
                    @?= Right (GameState [] [] []
                                         0 0 0
                                         [] [] [] []),
            testCase "normal" $ parseFrom state "( (players player_1 player-2) (supply copper copper estate province) (trash silver mine)\
                                                 \ (actions 1) (buys 1) (coins 0)\
                                                 \ (deck mine estate) (hand copper mine silver) (plays mine) (discards estate copper) )"
                    @?= Right (GameState ["player_1", "player-2"] [Copper, Copper, Estate, Province] [Silver, Mine]
                                         1 1 0
                                         [Mine, Estate] [Copper, Mine, Silver] [Mine] [Estate, Copper] )
        ],

        testGroup "notifications"
        [
            testCase "update normal" $ parseFrom notification "(moved john (clean copper))" @?= Right (Update "john" (Clean (Just Copper))),
            testCase "update spaces" $ parseFrom notification "( moved player-2 (clean copper) )" @?= Right (Update "player-2" (Clean (Just Copper))),
            testCase "update act" $ parseFrom notification "(moved player_1 (act mine copper silver))" @?= Right (Update "player_1" (Act Mine [Copper, Silver])),
            testCase "update moved" $ parseFrom notification "(move ((players) (supply) (trash) (actions 0) (buys 0) (coins 0) (deck) (hand) (plays) (discards)))" @?= Right (Request (GameState [] [] [] 0 0 0 [] [] [] []))
        ],

        testGroup "atomics"
        [
            testCase "normal name" $ parseFrom name "john" @?= Right "john",
            testCase "strange name" $ parseFrom name "007_agent-bond" @?= Right "007_agent-bond",
            testCase "0" $ parseFrom number "0" @?= Right 0,
            testCase "42" $ parseFrom number "42" @?= Right 42
        ],

        testGroup "cards"
        [
            testCase "copper" $ parseFrom card "copper" @?= Right Copper,
            testCase "silver" $ parseFrom card "silver" @?= Right Silver,
            testCase "gold" $ parseFrom card "gold" @?= Right Gold,

            testCase "estate" $ parseFrom card "estate" @?= Right Estate,
            testCase "duchy" $ parseFrom card "duchy" @?= Right Duchy,
            testCase "province" $ parseFrom card "province" @?= Right Province,

            testCase "mine" $ parseFrom card "mine" @?= Right Mine,
            testCase "cellar" $ parseFrom card "cellar" @?= Right Cellar,
            testCase "market" $ parseFrom card "market" @?= Right Market,
            testCase "remodel" $ parseFrom card "remodel" @?= Right Remodel,
            testCase "smithy" $ parseFrom card "smithy" @?= Right Smithy,
            testCase "village" $ parseFrom card "village" @?= Right Village,
            testCase "woodcutter" $ parseFrom card "woodcutter" @?= Right Woodcutter,
            testCase "workshop" $ parseFrom card "workshop" @?= Right Workshop
        ]
    ]



fullSuply = [Copper, Silver, Gold,
             Estate, Duchy, Province,
             Mine, Cellar, Market, Remodel, Smithy, Village, Woodcutter, Workshop]

mkState actions buys coins cards = GameState ["me", "other"] fullSuply [] actions buys coins [] cards [] []


agent = testGroup "agent"
    [
        testGroup "act"
        [
            testCase "mine" $ tryAction (mkState 1 1 0 [Mine, Copper]) @?= Left (Act Mine [Copper, Silver]),
            testCase "village" $ tryAction (mkState 1 1 0 [Mine, Smithy, Village, Copper]) @?= Left (Act Village []),
            testCase "smithy" $ tryAction (mkState 1 1 0 [Mine, Smithy, Copper]) @?= Left (Act Smithy [])
        ],

        testGroup "add"
        [
            testCase "copper" $ tryAdd (mkState 0 1 0 [Copper]) @?= Left (Add Copper),
            testCase "silver" $ tryAdd (mkState 0 1 0 [Silver]) @?= Left (Add Silver),
            testCase "gold" $ tryAdd (mkState 0 1 0 [Gold]) @?= Left (Add Gold),
            testCase "failure" $ tryAdd (mkState 0 1 0 []) @?= Right (mkState 0 1 0 [])
        ],

        testGroup "buy"
        [
            testCase "province" $ tryBuy (mkState 0 1 8 []) @?= Left (Buy Province),
            testCase "gold" $ tryBuy (mkState 0 1 6 []) @?= Left (Buy Gold),
            testCase "mine" $ tryBuy (mkState 0 1 5 [Copper]) @?= Left (Buy Mine),
            testCase "smithy" $ tryBuy (mkState 0 1 4 [Copper]) @?= Left (Buy Smithy),
            testCase "village" $ tryBuy (mkState 0 1 4 [Smithy]) @?= Left (Buy Village),
            testCase "silver" $ tryBuy (mkState 0 1 4 [Smithy, Village]) @?= Left (Buy Silver),
            testCase "duchy" $ tryBuy (mkState 0 1 5 [Smithy, Village, Mine]) @?= Left (Buy Duchy)
        ]
    ]



dataTests = testGroup "data"
    [
        testGroup "action show"
        [
            testCase "add" $ show (Add Gold) @?= "(add gold)",
            testCase "clean empty" $ show (Clean Nothing) @?= "(clean)",
            testCase "clean something" $ show (Clean (Just Duchy)) @?= "(clean duchy)",
            testCase "buy" $ show (Buy Province) @?= "(buy province)",
            testGroup "act"
            [
                testCase "mine" $ show (Act Mine [Silver, Gold]) @?= "(act mine silver gold)",
                testCase "cellar" $ show (Act Cellar [Province, Mine, Duchy, Copper]) @?= "(act cellar province mine duchy copper)",
                testCase "market" $ show (Act Market []) @?= "(act market)",
                testCase "remodel" $ show (Act Remodel [Gold, Province]) @?= "(act remodel gold province)",
                testCase "smithy" $ show (Act Smithy []) @?= "(act smithy)",
                testCase "village" $ show  (Act Village []) @?= "(act village)",
                testCase "woodcutter" $ show (Act Woodcutter []) @?= "(act woodcutter)",
                testCase "workshop" $ show (Act Workshop [Village]) @?= "(act workshop village)"
            ]
        ]
    ]
