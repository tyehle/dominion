module Main where

import Dominion
import Cards

import Control.Monad.State
import System.Random


main :: IO ()
-- main = putStrLn "This is the application" >> getLine >>= (\line -> getStdGen >>= print . evalState (buildVec . read $ line))
main = do
    plop "This is the application"
    putStrLn $ "Gold costs " ++ (show . cost $ Gold)
    line <- getLine
    gen <- getStdGen
    print . evalState (buildVec . read $ line) $ gen


buildVec :: Int -> State StdGen [Int]
buildVec 0 = return []
buildVec n = do
    gen <- get
    let (v, newGen) = randomR (1,10) gen in do
        put newGen
        rest <- buildVec (n-1)
        return (v:rest)

buildVec2 :: (Int, StdGen) -> ([Int], StdGen)
buildVec2 (0, gen) = ([], gen)
buildVec2 (n, gen) = let (v, newGen) = randomR (1,10) gen
                         (rest, finalGen) = buildVec2 (n-1, newGen) in
                         (v:rest, finalGen)
