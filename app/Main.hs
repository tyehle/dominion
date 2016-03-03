module Main where

import Parser (parseNotification)
import Agent (respond)

import System.IO


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hGetContents stdin >>= runClient


runClient :: String -> IO ()
runClient "" = return ()
runClient input = (putStr . respond) notification >> runClient moreInput
    where (notification, moreInput) = parseNotification input

