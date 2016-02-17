module Cards
(
    Card(..), cost, worth, coins
)
where



data Card = Cellar | Moat |
            Village | Woodcutter | Workshop |
            Militia | Remodel | Smithy |
            Market | Mine |
            -- Treasures
            Copper | Silver | Gold |
            -- Victory cards
            Estate | Duchy | Province deriving (Eq, Show)

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


worth :: Card -> Int
worth Copper = 1
worth Silver = 2
worth Gold   = 3



coins :: [Card] -> Int
coins [] = 0
coins (c:rest) = (worth c) + (coins rest)
