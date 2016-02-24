module Main where

import Parser
-- import Data
import qualified Agent as Bond

import Control.Monad.State
import System.Random


main :: IO ()
main = runClient


runClient :: IO ()
runClient = getLine >>= processLine >> runClient

processLine :: String -> IO ()
processLine line = case parseNotification line of
    Update name action -> return ()
    Request state      -> putStrLn . show $ Bond.act state
