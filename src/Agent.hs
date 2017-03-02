module Agent
  ( Agent
  , respond
  , act, defend
  , tryAction, tryAdd, tryBuy
  , tryDefend, discardTo
  , revealMoat, discardPriority, findDiscards
  , trySimplePlay, tryMine, inHand, canAct, canAdd
  , playAllTreasures
  , buyPriority
  , allMyCards, probDraw, expectedTreasure, expectedDrawValue, numInSupply
  ) where

import Data
import Data.List (find, partition)


class Agent a where
    tryAction :: a -> GameState -> Either Action GameState
    tryAdd :: a -> GameState -> Either Action GameState
    tryBuy :: a -> GameState -> Either Action GameState
    tryDefend :: a -> GameState -> Either Defense GameState
    discardTo :: a -> GameState -> Int -> Defense

respond :: (Agent a) => a -> Notification -> String
respond _ (Update name action) = ""
respond _ (Defended name how) = ""
respond a (Attacked how name state) = show (defend a how state)
respond a (Request state) = show (act a state)

act :: (Agent a) => a -> GameState -> Action
act a state = case tryAction a state >>= tryAdd a >>= tryBuy a of
    Left action  -> action
    Right _      -> Clean $ find (const True) (hand state)

defend :: (Agent a) => a -> Action -> GameState -> Defense
defend a (Act Militia []) state = fallback (tryDefend a state)
    where
        fallback (Left defense) = defense
        fallback (Right state) = discardTo a state 3
defend _ bad _ = error $ "unexpected attack: " ++ show bad


-- Defense --

revealMoat :: GameState -> Either Defense GameState
revealMoat state
    | inHand state Moat = Left $ Reveal Moat
    | otherwise = Right state

discardPriority :: [Card] -> GameState -> Int -> Defense
discardPriority priority state n = Discard (findDiscards (length (hand state) - n)
                                                         (hand state)
                                                         priority)

findDiscards :: Int -> [Card] -> [Card] -> [Card]
findDiscards n hand toTry
    | n <= 0     = []
    | null toTry = take n hand
    | otherwise  = take n toRemove ++ findDiscards (n - length toRemove) toKeep (tail toTry)
    where
        (toRemove, toKeep) = partition (== head toTry) hand

----



-- Actions --

trySimplePlay :: Card -> GameState -> Either Action GameState
trySimplePlay card state
    | canAct state card = Left $ Act card []
    | otherwise = Right state

tryMine :: GameState -> Either Action GameState
tryMine state
    | canAct state Mine && inHand state Silver && canAdd state Gold   = Left $ Act Mine [Silver, Gold]
    | canAct state Mine && inHand state Copper && canAdd state Silver = Left $ Act Mine [Copper, Silver]
    | otherwise = Right state


inHand :: GameState -> Card -> Bool
inHand state card = card `elem` hand state

canAct :: GameState -> Card -> Bool
canAct state card = actions state > 0 && inHand state card

canAdd :: GameState -> Card -> Bool
canAdd state card = card `elem` supply state

----



-- Adding Treasures --

playAllTreasures :: GameState -> Either Action GameState
playAllTreasures state = case find isTreasure (hand state) of
    Just t -> Left $ Add t
    _ -> Right state

----



-- Buying --

buyPriority :: (GameState -> Card -> Bool) -> [Card] -> GameState -> Either Action GameState
buyPriority should priority state
    | buys state > 0 = case find (\c -> canBuy c && should state c) priority of
        Just c -> Left (Buy c)
        Nothing -> Right state
    | otherwise = Right state
    where
        canBuy c = cost c <= coins state && c `elem` supply state

----



-- Utility Functions --

-- Gets all of the current players cards given a game state
allMyCards :: GameState -> [Card]
allMyCards state = concatMap ($ state) [deck, hand, plays, discards]

-- Computes the probability of drawing a single card from a list of cards
probDraw :: [Card] -> Card -> Double
probDraw allCards card = count / total
    where
        count = fromIntegral . length . filter (== card) $ allCards
        total = fromIntegral . length $ allCards

expectedTreasure :: (Fractional a) => [Card] -> a
expectedTreasure deck = sum (map treasureWorth deck) / fromIntegral (length deck)

expectedDrawValue :: (Fractional a) => GameState -> Int -> a
expectedDrawValue state n
    | n <= length (deck state) = expectedTreasure (deck state) * fromIntegral n
    | otherwise = deckWorth + expectedDiscardValue * discardsDrawn
    where
        deckWorth = sum . map treasureWorth . deck $ state
        expectedDiscardValue = expectedTreasure . discards $ state
        discardsDrawn = fromIntegral $ n - (length . deck $ state)

numInSupply :: GameState -> Card -> Int
numInSupply state card = length . filter (== card) . supply $ state
