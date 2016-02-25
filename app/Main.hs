module Main where

import Parser (parseNotification, Notification(..))
import Agent (act)


main :: IO ()
main = getLine >>= processLine >> main


processLine :: String -> IO ()
processLine line = case parseNotification line of
    Update name action -> return ()
    Request state      -> putStrLn . show $ act state
