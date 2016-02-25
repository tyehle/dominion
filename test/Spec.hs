
import Parser
import Data

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "all tests" [parsing]


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

            testCase "act" $ parseFrom play "(act mine copper silver)" @?= Right (Act Mine [Copper, Silver])
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
            testCase "update normal" $ parseNotification "(moved john (clean copper))" @?= Update "john" (Clean (Just Copper)),
            testCase "update spaces" $ parseNotification "( moved player-2 (clean copper) )" @?= Update "player-2" (Clean (Just Copper)),
            testCase "update act" $ parseNotification "(moved player_1 (act mine copper silver))" @?= Update "player_1" (Act Mine [Copper, Silver]),
            testCase "update moved" $ parseNotification "(move ((players) (supply) (trash) (actions 0) (buys 0) (coins 0) (deck) (hand) (plays) (discards)))" @?= Request (GameState [] [] [] 0 0 0 [] [] [] [])
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

            testCase "mine" $ parseFrom card "mine" @?= Right Mine
        ]
    ]
