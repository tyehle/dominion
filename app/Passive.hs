module Main where

import AgentInstances (Passive(..), drive)


main :: IO ()
main = drive Passive
