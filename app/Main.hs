module Main where

import Parser (parseNotification)
import Agent (respond)

import Data.Char (isSpace)
import System.IO


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hGetContents stdin >>= runClient


runClient :: String -> IO ()
runClient input
    | null trimmed = return ()
    | otherwise    = (putStr . respond) notification >> runClient moreInput
    where
        trimmed = dropWhile isSpace input
        (notification, moreInput) = parseNotification trimmed
