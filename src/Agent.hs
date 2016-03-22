module Agent
(
    respond,
    act, defend,
    discardTo, findDiscards,
    tryAction, tryMine,
    tryAdd,
    tryBuy
)
where

import Data
import Data.List (find, partition)

respond :: Notification -> String
respond (Update name action) = ""
respond (Defended name how) = ""
respond (Attacked how name state) = show (defend how state)
respond (Request state) = show . act $ state

act :: GameState -> Action
act state = case tryAction state >>= tryAdd >>= tryBuy of
    Left a  -> a
    Right _ -> Clean $ find (\_ -> True) (hand state)

defend :: Action -> GameState -> Defense
defend (Act Militia []) state
    | inHand state Moat = Reveal Moat
    | otherwise         = discardTo 3 state
defend _ _ = error "unexpected attack!"



-- Defense --

discardTo :: Int -> GameState -> Defense
discardTo n state = Discard (findDiscards (length (hand state) - n)
                                          (hand state)
                                          [Province, Duchy, Estate, Copper, Mine, Village, Silver, Smithy, Gold])

findDiscards :: Int -> [Card] -> [Card] -> [Card]
findDiscards n hand toTry
    | n <= 0     = []
    | null toTry = take n hand
    | otherwise  = (take n toRemove) ++ findDiscards (n - length toRemove) toKeep (tail toTry)
    where (toRemove, toKeep) = partition (== (head toTry)) hand

----



-- Actions --

tryAction :: GameState -> Either Action GameState
tryAction state = tryVillage state >>= trySmithy >>= tryMine


tryVillage :: GameState -> Either Action GameState
tryVillage state
    | canPlay state Village = Left $ Act Village []
    | otherwise = Right state

trySmithy :: GameState -> Either Action GameState
trySmithy state
    | canPlay state Smithy = Left $ Act Smithy []
    | otherwise = Right state

tryMine :: GameState -> Either Action GameState
tryMine state
    | canPlay state Mine && inHand state Silver && canAdd state Gold   = Left $ Act Mine [Silver, Gold]
    | canPlay state Mine && inHand state Copper && canAdd state Silver = Left $ Act Mine [Copper, Silver]
    | otherwise = Right state


inHand :: GameState -> Card -> Bool
inHand state card = card `elem` hand state

canPlay :: GameState -> Card -> Bool
canPlay state card = actions state > 0 && inHand state card

canAdd :: GameState -> Card -> Bool
canAdd state card = card `elem` supply state

----



-- Adding Treasures --

tryAdd :: GameState -> Either Action GameState
tryAdd state = case find isTreasure (hand state) of
    Just t -> Left $ Add t
    _ -> Right state

----



-- Buying --

tryBuy :: GameState -> Either Action GameState
tryBuy state
    | buys state > 0 = case find (\c -> canBuy c && shouldBuy state c) cardPriority of
        Just c -> Left $ Buy c
        Nothing -> Right state
    | otherwise = Right state
    where   canBuy c = cost c <= coins state && c `elem` supply state
            cardPriority = [Province, Gold, Mine, Smithy, Village, Duchy, Silver, Copper]

shouldBuy :: GameState -> Card -> Bool
shouldBuy _ Province = True
shouldBuy state Mine = deckPDraw (allMyCards state) Mine  < 0.15
shouldBuy _ Gold = True
shouldBuy state Smithy = deckPDraw (allMyCards state) Smithy < 0.08
shouldBuy state Village = deckPDraw (allMyCards state) Village < 0.1
shouldBuy _ Duchy = True
shouldBuy _ Silver = True
shouldBuy _ Copper = False

----



-- Utility Functions --

-- Gets all of the current players cards given a game state
allMyCards :: GameState -> [Card]
allMyCards state = concat $ map ($ state) [deck, hand, plays, discards]

-- Computes the probability of drawing a single card from a list of cards
deckPDraw :: [Card] -> Card -> Double
deckPDraw allCards card = count / total
    where count = fromIntegral . length . filter (== card) $ allCards
          total = fromIntegral . length $ allCards

expectedTreasure :: (Fractional a) => [Card] -> a
expectedTreasure deck = (sum (map treasureWorth deck)) / (fromIntegral (length deck))
