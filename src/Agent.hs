module Agent
(
    respond,
    act,
    tryAction, tryMine,
    tryAdd,
    tryBuy
)
where

import Data
import Data.List (find)

respond :: Notification -> String
respond (Update name action) = ""
respond (Request state) = show . act $ state

act :: GameState -> Action
act state = case tryAction state >>= tryAdd >>= tryBuy of
    Left a  -> a
    Right _ -> Clean $ find (\_ -> True) (hand state)


tryAction :: GameState -> Either Action GameState
tryAction state = tryMine state

tryMine :: GameState -> Either Action GameState
tryMine state
    | canDo && inHand Silver && inSupply Gold = Left $ Act Mine [Silver, Gold]
    | canDo && inHand Copper && inSupply Silver = Left $ Act Mine [Copper, Silver]
    | otherwise = Right state
    where canDo = actions state > 0 && inHand Mine
          inHand c = c `elem` hand state
          inSupply c = c `elem` supply state



tryAdd :: GameState -> Either Action GameState
tryAdd state = case find isTreasure (hand state) of
    Just t -> Left $ Add t
    _ -> Right state


tryBuy :: GameState -> Either Action GameState
tryBuy state
    | buys state > 0 = case find (\c -> canBuy c && shouldBuy state c) [Province, Mine, Gold, Duchy, Silver, Copper] of
        Just c -> Left $ Buy c
        Nothing -> Right state
    | otherwise = Right state
    where canBuy c = cost c <= coins state && c `elem` supply state

shouldBuy :: GameState -> Card -> Bool
shouldBuy _ Province = True
shouldBuy state Mine = deckPDraw (allMyCards state) Mine  < 0.15
shouldBuy _ Gold = True
shouldBuy _ Duchy = True
shouldBuy _ Silver = True
shouldBuy _ Copper = False
shouldBuy _ Estate = False


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
