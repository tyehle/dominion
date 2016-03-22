
import Parser
import Data
import Agent

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "all tests" [parsing, agent, dataTests]


stateString = "( (players player_1 player-2) (supply copper copper estate province) (trash silver mine)\
               \ (actions 1) (buys 1) (coins 0)\
               \ (deck mine estate) (hand copper mine silver) (plays mine) (discards estate copper) )"
stateObj = GameState ["player_1", "player-2"] [Copper, Copper, Estate, Province] [Silver, Mine]
                      1 1 0
                      [Mine, Estate] [Copper, Mine, Silver] [Mine] [Estate, Copper]

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
                testCase "workshop" $ parseFrom play "(act workshop village)" @?= Right (Act Workshop [Village]),
                testCase "militia" $ parseFrom play "(act militia)" @?= Right (Act Militia []),
                testCase "moat" $ parseFrom play "(act moat)" @?= Right (Act Moat [])
            ]
        ],

        testGroup "state"
        [
            testCase "with empties" $ parseFrom state "((players) (supply) (trash) (actions 0) (buys 0) (coins 0) (deck) (hand) (plays) (discards))"
                    @?= Right (GameState [] [] []
                                         0 0 0
                                         [] [] [] []),
            testCase "normal" $ parseFrom state stateString @?= Right stateObj
        ],

        testGroup "notifications"
        [
            testCase "update normal" $ parseFrom notification "(moved john (clean copper))" @?= Right (Update "john" (Clean (Just Copper))),
            testCase "update spaces" $ parseFrom notification "( moved player-2 (clean copper) )" @?= Right (Update "player-2" (Clean (Just Copper))),
            testCase "request" $ parseFrom notification "(moved player_1 (act mine copper silver))" @?= Right (Update "player_1" (Act Mine [Copper, Silver])),
            testCase "update move" $ parseFrom notification "(move ((players) (supply) (trash) (actions 0) (buys 0) (coins 0) (deck) (hand) (plays) (discards)))" @?= Right (Request (GameState [] [] [] 0 0 0 [] [] [] [])),
            testCase "attacked" $ parseFrom notification ( "(attacked (act militia) tim " ++ stateString ++ ")" ) @?= Right (Attacked (Act Militia []) "tim" stateObj),
            testCase "attacked spaces" $ parseFrom notification ( "( attacked (act militia) tim " ++ stateString ++ " )" ) @?= Right (Attacked (Act Militia []) "tim" stateObj),
            testCase "defended" $ parseFrom notification "(defended tim (moat))" @?= Right (Defended "tim" (Reveal Moat)),
            testCase "defended spaces" $ parseFrom notification "( defended tim ( discard copper estate ) )" @?= Right (Defended "tim" (Discard [Copper, Estate]))
        ],

        testGroup "defense"
        [
            testCase "reveal moat" $ parseFrom defense "(moat)" @?= Right (Reveal Moat),
            testCase "reveal estate" $ parseFrom defense "(estate)" @?= Right (Reveal Estate),
            testCase "discard cards" $ parseFrom defense "(discard province mine)" @?= Right (Discard [Province, Mine]),
            testCase "discard nothing" $ parseFrom defense "(discard)" @?= Right (Discard []),
            testCase "discard nothing spaces" $ parseFrom defense "( discard  )" @?= Right (Discard [])
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
            testCase "workshop" $ parseFrom card "workshop" @?= Right Workshop,
            testCase "militia" $ parseFrom card "militia" @?= Right Militia,
            testCase "moat" $ parseFrom card "moat" @?= Right Moat
        ],

        testGroup "external interface"
        [
            testCase "with spaces" $ parseNotification "( moved john (clean) )   more" @?= (Update "john" (Clean Nothing), "   more"),
            testCase "no spaces" $ parseNotification "(moved john (clean))()" @?= (Update "john" (Clean Nothing), "()")
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
        ],

        testGroup "defend"
        [
            testCase "reveal moat" $ defend (Act Militia []) (mkState 1 1 0 [Estate, Moat, Copper]) @?= Reveal Moat,
            testCase "discard to 3" $ defend (Act Militia []) (mkState 1 1 0 [Estate, Copper, Copper, Mine, Silver]) @?= Discard [Estate, Copper]
        ],

        testGroup "discard to"
        [
            testCase "victory cards" $ discardTo 2 (mkState 1 1 0 [Estate, Duchy, Province, Copper, Gold]) @?= Discard [Province, Duchy, Estate],
            testCase "other cards" $ discardTo 2 (mkState 1 1 0 [Silver, Silver, Copper, Village, Smithy]) @?= Discard [Copper, Village, Silver]
        ],

        testGroup "find discards"
        [
            testCase "done case" $ findDiscards 0 [Copper] [Copper] @?= [],
            testCase "extra done" $ findDiscards (-1) [Copper] [Copper] @?= [],
            testCase "none left" $ findDiscards 2 [Cellar, Moat, Workshop] [] @?= [Cellar, Moat],
            testCase "regular" $ findDiscards 2 [Gold, Copper, Duchy] [Duchy, Copper] @?= [Duchy, Copper],
            testCase "duplicates" $ findDiscards 3 [Gold, Duchy, Copper, Duchy] [Duchy, Copper] @?= [Duchy, Duchy, Copper],
            testCase "too many" $ findDiscards 1 [Gold, Copper, Copper, Copper] [Copper] @?= [Copper]
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
            testCase "act mine" $ show (Act Mine [Silver, Gold]) @?= "(act mine silver gold)",
            testCase "act smithy" $ show (Act Smithy []) @?= "(act smithy)"
        ],

        testGroup "defense show"
        [
            testCase "reveal moat" $ show (Reveal Moat) @?= "(moat)",
            testCase "reveal estate" $ show (Reveal Estate) @?= "(estate)",
            testCase "discard nothing" $ show (Discard []) @?= "(discard)",
            testCase "discard cards" $ show (Discard [Copper, Estate]) @?= "(discard copper estate)"
        ]
    ]
