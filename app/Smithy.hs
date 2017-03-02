module Main where

import AgentInstances (SmithyMoney(..), drive)


main :: IO ()
main = drive SmithyMoney
