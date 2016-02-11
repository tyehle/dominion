module Main where

import Dominion

import Control.Monad.State
import System.Random


main :: IO ()
main = do
    plop "This is the application" >> getLine >>= (\line -> getStdGen >>= (print . evalState (buildVec . read $ line)))


buildVec :: Int -> State StdGen [Int]
buildVec 0 = return []
buildVec n = do
    gen <- get
    let (v, newGen) = randomR (1,10) gen in do
        put newGen
        rest <- buildVec (n-1)
        return (v:rest)
