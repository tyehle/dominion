module Data
(
    GameState(..), Action(..), Card(..),
    cost, worth,
    isTreasure, isAction, isVictory
)
where


import Data.List (intercalate)
import Data.Char (toLower)


data GameState = GameState { players :: [String], supply :: [Card], trash :: [Card]
                           , actions :: Int, buys :: Int, coins :: Int
                           ,deck :: [Card], hand :: [Card], plays :: [Card], discards :: [Card]
                           } deriving (Eq, Show)

data Action = Add Card | Clean (Maybe Card) | Buy Card | Act Card [Card] deriving (Eq)

data Card = Cellar | Moat |
            Village | Woodcutter | Workshop |
            Militia | Remodel | Smithy |
            Market | Mine |
            -- Treasures
            Copper | Silver | Gold |
            -- Victory cards
            Estate | Duchy | Province deriving (Eq, Show)


-- Wraps a list of strings in parens, and lower cases all of them
wrap :: [String] -> String
wrap ss = "(" ++ intercalate " " (map (map toLower) ss) ++ ")"

instance Show Action where
    show (Add c) = wrap ["add", show c]
    show (Clean (Just c)) = wrap ["clean", show c]
    show (Clean Nothing) = "(clean)"
    show (Buy c) = wrap ["buy", show c]
    show (Act c cs) = wrap $ "act" : show c : map show cs



isTreasure :: Card -> Bool
isTreasure Copper = True
isTreasure Silver = True
isTreasure Gold   = True
isTreasure _      = False

isVictory :: Card -> Bool
isVictory Estate   = True
isVictory Duchy    = True
isVictory Province = True
isVictory _        = False

isAction :: Card -> Bool
isAction Cellar = True
isAction Moat   = True
isAction Village    = True
isAction Woodcutter = True
isAction Workshop   = True
isAction Militia = True
isAction Remodel = True
isAction Smithy  = True
isAction Market = True
isAction Mine   = True



cost :: Card -> Int
cost Cellar = 2
cost Moat   = 2
cost Village    = 3
cost Woodcutter = 3
cost Workshop   = 3
cost Militia = 4
cost Remodel = 4
cost Smithy  = 4
cost Market = 5
cost Mine   = 5
-- Treasures
cost Copper = 0
cost Silver = 3
cost Gold   = 6
-- Victory Cards
cost Estate   = 2
cost Duchy    = 5
cost Province = 8


worth :: Card -> Int
worth Copper = 1
worth Silver = 2
worth Gold   = 3
-- Victory Cards
worth Estate   = 1
worth Duchy    = 3
worth Province = 6
