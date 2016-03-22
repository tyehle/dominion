module Parser
(
    parseFrom, labeledList,
    parseNotification,
    state, card, treasure, victory, action, play, notification, defense, name, number
)
where

import Data

import Text.ParserCombinators.Parsec


-- parser interface

parseFrom :: GenParser Char () a -> String -> Either ParseError a
parseFrom rule input = parse rule "function-argument" input

parseNotification :: String -> (Notification, String)
parseNotification input
    | Left e       <- parsed = error . show $ e
    | Right result <- parsed = result
    where parsed = parse (withRemainingNoSpace notification) "stdin" input

----



-- convenience functions

withRemainingNoSpace :: Parser a -> Parser (a, String)
withRemainingNoSpace p = do
    result <- spaces *> p <* spaces
    rest <- getInput
    return (result, rest)

inParens :: GenParser Char st a -> GenParser Char st a
inParens p = char '(' *> spaces *> p <* spaces <* char ')'

word :: String -> GenParser Char st String
word = try . string

labeledList :: String -> GenParser Char st a -> GenParser Char st [a]
labeledList label items = prefixedList (word label) items >>= \(_, list) -> return list

prefixedList :: GenParser Char st p -> GenParser Char st a -> GenParser Char st (p, [a])
prefixedList label items = inParens $ label >>= \pre -> itemList >>= return . (,) pre
    where itemList = try (many1 space >> sepEndBy items (many1 space)) <|> return []

----



-- The actual grammar

state :: GenParser Char st GameState
state = inParens $ do {
        players <- many space >> labeledList "players" name;
        supply  <- many1 space >> labeledList "supply" card;
        trash   <- many1 space >> labeledList "trash"  card;

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
action = (word "mine" >> return Mine)
     <|> (word "cellar" >> return Cellar)
     <|> (word "market" >> return Market)
     <|> (word "remodel" >> return Remodel)
     <|> (word "smithy" >> return Smithy)
     <|> (word "village" >> return Village)
     <|> (word "woodcutter" >> return Woodcutter)
     <|> (word "workshop" >> return Workshop)
     <|> (word "militia" >> return Militia)
     <|> (word "moat" >> return Moat)


play :: GenParser Char st Action
play =  (try (prefixedList actionPrefix card >>= return . buildAction))
    <|> (inParens $ ( word "add" >> many1 space >> treasure >>= return . Add )
                <|> ( word "buy" >> many1 space >> card >>= return . Buy )
                <|> ( word "clean" >> optionMaybe (many1 space >> card) >>= return . Clean ))
    where   actionPrefix = word "act" >> many1 space >> action
            buildAction (played, cards) = Act played cards


notification :: GenParser Char st Notification
notification = inParens $ update <|> request <|> attacked <|> defended

update =
    do {
        word "moved" >> many1 space;
        n <- name <* many1 space;
        p <- play;
        return $ Update n p
    }

request = word "move" >> many1 space >> state >>= return . Request

attacked =
    do {
        word "attacked" >> many1 space;
        act <- play <* many1 space;
        n <- name <* many1 space;
        s <- state;
        return $ Attacked act n s
    }

defended =
    do {
        word "defended" >> many1 space;
        n <- name <* many1 space;
        d <- defense;
        return $ Defended n d
    }

defense :: GenParser Char st Defense
defense =  try ( inParens card >>= return . Reveal )
       <|> ( labeledList "discard" card >>= return . Discard )


name :: GenParser Char st String
name = many1 (oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-_"))

number :: GenParser Char st Int
number = many1 digit >>= return . read
