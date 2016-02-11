
module Dominion
(
    plop
)
where

import System.Random

data Action = Act | Buy deriving (Eq, Ord, Enum)

data Player = Player { hand :: [Card],
                       discard :: [Card],
                       deck :: [Card],
                       actions :: Int,
                       coins :: Int,
                       buys :: Int,
                       points :: Int
                     }

data State = State {bank :: [(Card, Int)], players :: [Player], phase :: Action}

data Card = Card {title :: Int, cost :: Int, action :: State -> State}


plop :: String -> IO ()
plop [] = putChar '\n' >> return ()
plop (c:cs) = putChar c >> plop cs