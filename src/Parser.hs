module Parser
  ( parseFrom, labeledList
  , parseNotification
  , state, card, treasure, victory, action, play, notification, defense, name, number
  ) where

import Data

import Text.ParserCombinators.Parsec


-- parser interface

parseFrom :: GenParser Char () a -> String -> Either ParseError a
parseFrom rule = parse rule "function-argument"

parseNotification :: String -> (Notification, String)
parseNotification input
    | Left e       <- parsed = error . show $ e
    | Right result <- parsed = result
    where
        parsed = parse (withRemaining notification) "stdin" input

----



-- convenience functions

withRemaining :: Parser a -> Parser (a, String)
withRemaining p = do
    result <- p
    rest <- getInput
    return (result, rest)

inParens :: GenParser Char st a -> GenParser Char st a
inParens p = char '(' *> spaces *> p <* spaces <* char ')'

word :: String -> GenParser Char st String
word = try . string

labeledList :: String -> GenParser Char st a -> GenParser Char st [a]
labeledList label items = prefixedList (word label) items >>= \(_, list) -> return list

prefixedList :: GenParser Char st p -> GenParser Char st a -> GenParser Char st (p, [a])
prefixedList label items = inParens $ do
  pre <- label
  items <- itemList
  return (pre, items)
  where
    itemList = try (many1 space >> sepEndBy items (many1 space)) <|> return []

----



-- The actual grammar

state :: GenParser Char st GameState
state = inParens $ do
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
play =  try (buildAction <$> prefixedList actionPrefix card)
    <|> inParens ((Add <$> (word "add" >> many1 space >> treasure))
              <|> (Buy <$> (word "buy" >> many1 space >> card))
              <|> (Clean <$> (word "clean" >> optionMaybe (many1 space >> card))))
    where
        actionPrefix = word "act" >> many1 space >> action
        buildAction (played, cards) = Act played cards


notification :: GenParser Char st Notification
notification = inParens $ update <|> request <|> attacked <|> defended

update = do
    word "moved" >> many1 space;
    n <- name <* many1 space;
    p <- play;
    return $ Update n p

request = Request <$> (word "move" >> many1 space >> state)

attacked = do
    word "attacked" >> many1 space;
    act <- play <* many1 space;
    n <- name <* many1 space;
    s <- state;
    return $ Attacked act n s

defended = do
    word "defended" >> many1 space;
    n <- name <* many1 space;
    d <- defense;
    return $ Defended n d

defense :: GenParser Char st Defense
defense =  try ( Reveal <$> inParens card )
       <|> ( Discard <$> labeledList "discard" card )


name :: GenParser Char st String
name = many1 (oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-_"))

number :: GenParser Char st Int
number = read <$> many1 digit
