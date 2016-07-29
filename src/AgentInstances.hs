module AgentInstances
(
    Miner49er(..), Passive(..), SmithyMoney(..),
    drive, runClient
)
where

import Parser (parseNotification)
import Data
import Agent

import Data.List (sortOn)
import Data.Char (isSpace)
import System.IO


-- Functions for running an agent --

drive :: (Agent a) => a -> IO ()
drive agent = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hGetContents stdin >>= runClient agent


runClient :: (Agent a) => a -> String -> IO ()
runClient agent input
    | null trimmed = return ()
    | otherwise    = (putStr . respond agent) notification
                     >> runClient agent moreInput
    where
        trimmed = dropWhile isSpace input
        (notification, moreInput) = parseNotification trimmed

----





data Miner49er = Miner49er

instance Agent Miner49er where
    tryAction _ state = trySimplePlay Village state >>= trySimplePlay Militia >>= trySimplePlay Smithy >>= tryMine

    tryAdd _ = playAllTreasures

    tryBuy _ = buyPriority shouldBuy priority
        where
            priority = [Province, Gold, Mine, Militia, Smithy, Village, Duchy, Silver, Copper]

            shouldBuy _ Province = True
            shouldBuy _ Gold = True
            shouldBuy state Mine = probDraw (allMyCards state) Mine  < 0.15
            shouldBuy state Militia = length (filter (== Militia) (allMyCards state)) < 1
            shouldBuy state Smithy = probDraw (allMyCards state) Smithy < 0.08
            shouldBuy state Village = probDraw (allMyCards state) Village < 0.1
            shouldBuy _ Duchy = True
            shouldBuy _ Silver = True
            shouldBuy _ Copper = False

    tryDefend _ = revealMoat

    discardTo _ = discardPriority [Province, Duchy, Estate, Copper, Mine, Village, Silver, Smithy, Gold]





data Passive = Passive

instance Agent Passive where
    tryAction _ state = Right state
    tryAdd _ state = Right state
    tryBuy _ state = Right state
    tryDefend _ state = Right state
    discardTo _ = discardPriority []





data SmithyMoney = SmithyMoney

instance Agent SmithyMoney where
    tryAction _ = trySimplePlay Smithy

    tryAdd _ = playAllTreasures

    tryBuy _ = buyPriority shouldBuy priority
        where
            priority = [Province, Gold, Smithy, Duchy, Silver, Estate]
            shouldBuy _ Province = True
            shouldBuy state Gold = numInSupply state Province > 4
            shouldBuy state Smithy = probDraw (allMyCards state) Smithy < 0.05
            shouldBuy state Duchy = numInSupply state Province < 6
            shouldBuy state Silver = numInSupply state Province > 4
            shouldBuy state Estate = numInSupply state Province < 4

    tryDefend _ = revealMoat

    discardTo _ state = discardPriority ([Province, Duchy, Estate] ++ valueSorted) state
        where
            valueSorted = sortOn value [Copper, Silver, Gold, Smithy]
            value Smithy = expectedDrawValue state 3
            value t = treasureWorth t
