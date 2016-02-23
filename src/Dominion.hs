module Dominion
(
    plop, notification, zoom
)
where

import Cards

import Text.ParserCombinators.Parsec



data Action = Add Card | Clean (Maybe Card) | Buy Card | Act Card [Card] deriving (Eq, Show)

data GameState = GameState { players :: [String], supply :: [Card], trash :: [Card]
                           , actions :: Int, buys :: Int, coins :: Int
                           ,deck :: [Card], hand :: [Card], plays :: [Card], discards :: [Card]
                           }




zoom :: GenParser Char () a -> String -> Either ParseError a
zoom rule input = parse rule "function-argument" input

plop :: String -> IO ()
plop []     = return ()
plop (c:cs) = putChar c >> plop cs


state :: GenParser Char st GameState
state = inParens $ do {
        players <- labeled "players" $ sepBy1 name (many1 space);
        supply  <- labeled "supply" cardList;
        trash   <- labeled "trash" cardList;

        actions <- labeled "actions" $ number;
        buys    <- labeled "buys"    $ number;
        coins   <- labeled "coins"   $ number;

        deck     <- labeled "deck"     $ cardList;
        hand     <- labeled "hand"     $ cardList;
        plays    <- labeled "plays"    $ cardList;
        discards <- labeled "discards" $ cardList;

        return $ GameState players supply trash actions buys coins deck hand plays discards
    }

labeled :: String -> GenParser Char st a -> GenParser Char st a
labeled label p = inParens $ string label >> many1 space >> p

cardList :: GenParser Char st [Card]
cardList = inParens $ sepBy card (many1 space)


card :: GenParser Char st Card
card = treasure <|> victory <|> action

treasure :: GenParser Char st Card
treasure = (string "copper" >> return Copper) <|> (string "silver" >> return Silver) <|> (string "gold" >> return Gold)

victory :: GenParser Char st Card
victory = (string "estate" >> return Estate) <|> (string "duchy" >> return Duchy) <|> (string "province" >> return Province)

action :: GenParser Char st Card
action = string "mine" >> return Mine


inParens :: GenParser Char st a -> GenParser Char st a
inParens p = char '(' >> many1 space >> p <* many1 space <* char ')'


play :: GenParser Char st Action
-- play = card >> return (Clean Nothing)
play = inParens $ do {
        string "act" >> many1 space;
        string "mine" >> many1 space;
        t1 <- treasure;
        many1 space;
        t2 <- treasure;
        return $ Act Mine [t1, t2]
    }
    <|> ( string "add" >> many1 space >> treasure >>= return . Add )
    <|> ( string "buy" >> many1 space >> card >>= return . Buy )
    <|> ( string "clean" >> many1 space >> optionMaybe card >>= return . Clean )


-- don't parse the (move state) messages for the server
notification :: GenParser Char st (String, Action)
notification = inParens $ do {
        string "moved" >> many1 space;
        n <- name;
        many1 space;
        p <- play;
        return (n, p)
    }


name :: GenParser Char st String
name = many1 letter

number :: GenParser Char st Int
number = many1 digit >>= return . read
