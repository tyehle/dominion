module Main where

import Parser (parseNotification, Notification(..))
import Agent (act)

import System.IO


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hGetContents stdin >>= runClient


runClient :: String -> IO ()
runClient input = processMessage input >>= runClient


processMessage :: String -> IO String
processMessage input
    | Update name action <- notification = return otherInput
    | Request state <- notification      = (putStrLn . show . act) state >> return otherInput
    where (notification, otherInput) = parseNotification input

