module Dominion
(
    plop, notification, zoom
)
where

import Cards

import Text.ParserCombinators.Parsec
-- import Control.Monad.Identity (Identity)

data Action = Act Card | Clean (Maybe Card) | Buy deriving (Eq, Show)

-- data Player = Player { hand :: [Card],
--                        discard :: [Card],
--                        deck :: [Card],
--                        actions :: Int,
--                        coins :: Int,
--                        buys :: Int,
--                        points :: Int
--                      }

-- data State = State {bank :: [(Card, Int)], players :: [Player], phase :: Action}

zoom :: GenParser Char () a -> String -> Either ParseError a
zoom rule input = parse rule "function-argument" input

plop :: String -> IO ()
plop []     = return ()
plop (c:cs) = putChar c >> plop cs



card :: GenParser Char st Card
card = treasure <|> victory <|> action

treasure :: GenParser Char st Card
treasure = (string "copper" >> return Copper) <|> (string "silver" >> return Silver) <|> (string "gold" >> return Gold)

victory :: GenParser Char st Card
victory = (string "estate" >> return Estate) <|> (string "duchy" >> return Duchy) <|> (string "province" >> return Province)

action :: GenParser Char st Card
action = string "mine" >> return Mine

-- tmp
play = card

-- don't parse the (move state) messages for the server
notification :: GenParser Char st (String, Action)
notification = do
    char '('
    skipMany space
    string "moved"
    skipMany space
    n <- name
    skipMany space
    p <- play
    skipMany space
    char ')'
    return (n, (Act p))


name :: GenParser Char st String
name = many1 letter

number :: GenParser Char st Int
number = many1 digit >>= return . read
