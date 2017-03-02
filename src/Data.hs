module Data
  ( Notification(..), Defense(..), GameState(..), Action(..), Card(..)
  , cost
  , treasureWorth, victoryWorth
  , isTreasure, isAction
  ) where


import Data.List (intercalate)
import Data.Char (toLower)


data Notification = Request GameState
                  | Update String Action
                  | Attacked Action String GameState
                  | Defended String Defense deriving (Eq, Show)


data Defense = Reveal Card | Discard [Card] deriving (Eq)


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
wrap ss = "(" ++ unwords (map (map toLower) ss) ++ ")"

instance Show Action where
    show (Add c) = wrap ["add", show c]
    show (Clean (Just c)) = wrap ["clean", show c]
    show (Clean Nothing) = "(clean)"
    show (Buy c) = wrap ["buy", show c]
    show (Act c cs) = wrap $ "act" : show c : map show cs

instance Show Defense where
    show (Reveal c) = wrap [show c]
    show (Discard cs) = wrap $ "discard" : map show cs


isTreasure :: Card -> Bool
isTreasure Copper = True
isTreasure Silver = True
isTreasure Gold   = True
isTreasure _      = False

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
isAction _ = False



cost :: (Num a) => Card -> a
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
-- Treasures --
cost Copper = 0
cost Silver = 3
cost Gold   = 6
-- Victory Cards --
cost Estate   = 2
cost Duchy    = 5
cost Province = 8


treasureWorth :: (Num a) => Card -> a
treasureWorth Copper = 1
treasureWorth Silver = 2
treasureWorth Gold   = 3
treasureWorth _ = 0

victoryWorth :: (Num a) => Card -> a
victoryWorth Estate   = 1
victoryWorth Duchy    = 3
victoryWorth Province = 6
victoryWorth _ = 0
