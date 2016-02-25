module Parser
(
    parseFrom, labeledList,
    Notification(..),
    parseNotification,
    state, card, treasure, victory, action, play, notification, name, number
)
where

import Data

import Text.ParserCombinators.Parsec


data Notification = Request GameState | Update String Action deriving (Eq, Show)



-- exported functions

parseFrom :: GenParser Char () a -> String -> Either ParseError a
parseFrom rule input = parse rule "function-argument" input

parseNotification :: String -> Notification
parseNotification s = case parse notification "stdin" s of
    Left e       -> error . show $ e
    Right result -> result

----



-- convenience functions

inParens :: GenParser Char st a -> GenParser Char st a
inParens p = between (char '(') (char ')') $ spaces *> p <* spaces

word :: String -> GenParser Char st String
word = try . string

labeledList :: String -> GenParser Char st a -> GenParser Char st [a]
labeledList label items = inParens $ word label >> (try (many1 space >> sepEndBy items (many1 space)) <|> return [])

----



-- The actual grammar

state :: GenParser Char st GameState
state = inParens $ do {
        players <- many space >> labeledList "players" name;
        supply  <- many1 space >> labeledList "supply"  card;
        trash   <- many1 space >> labeledList "trash"   card;

        actions <- many1 space >> inParens ( word "actions" >> many1 space >> number );
        buys    <- many1 space >> inParens ( word "buys"    >> many1 space >> number );
        coins   <- many1 space >> inParens ( word "coins"   >> many1 space >> number );

        deck     <- many1 space >> labeledList "deck"     card;
        hand     <- many1 space >> labeledList "hand"     card;
        plays    <- many1 space >> labeledList "plays"    card;
        discards <- many1 space >> labeledList "discards" card;

        return $ GameState players supply trash actions buys coins deck hand plays discards
    }


card :: GenParser Char st Card
card = treasure <|> victory <|> action

treasure :: GenParser Char st Card
treasure = (word "copper" >> return Copper)
       <|> (word "silver" >> return Silver)
       <|> (word "gold" >> return Gold)

victory :: GenParser Char st Card
victory = (word "estate" >> return Estate)
      <|> (word "duchy" >> return Duchy)
      <|> (word "province" >> return Province)

action :: GenParser Char st Card
action = word "mine" >> return Mine


play :: GenParser Char st Action
play = inParens $ do {
        word "act" >> many1 space;
        word "mine" >> many1 space;
        t1 <- treasure;
        many1 space;
        t2 <- treasure;
        return $ Act Mine [t1, t2]
    }
    <|> ( word "add" >> many1 space >> treasure >>= return . Add )
    <|> ( word "buy" >> many1 space >> card >>= return . Buy )
    <|> ( word "clean" >> optionMaybe (many1 space >> card) >>= return . Clean )


notification :: GenParser Char st Notification
notification = inParens $ do {
        word "moved" >> many1 space;
        n <- name;
        many1 space;
        p <- play;
        return $ Update n p
    }
    <|> ( word "move" >> many1 space >> state >>= return . Request )


name :: GenParser Char st String
name = many1 (oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-_"))

number :: GenParser Char st Int
number = many1 digit >>= return . read
