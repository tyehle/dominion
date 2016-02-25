module Main where

import Parser (parseNotification, Notification(..))
import Agent (act)

import System.IO


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    runClient


runClient :: IO ()
runClient = readSExp >>= processMessage >> runClient


processMessage :: String -> IO ()
processMessage message = case parseNotification message of
        Update name action -> return ()
        Request state      -> putStrLn . show . act $ state


readSExp :: IO String
readSExp = eatUntil '(' >> readUntilClosed 1 >>= return . ('(' :)
    where eatUntil c = getChar >>= (\cur -> if cur == c then return () else eatUntil c)

readUntilClosed :: Int -> IO String
readUntilClosed 0 = return ""
readUntilClosed n = do
    cur <- getChar
    case cur of
        '(' -> readUntilClosed (n+1) >>= return . (cur :)
        ')' -> readUntilClosed (n-1) >>= return . (cur :)
        _   -> readUntilClosed n     >>= return . (cur :)
