module Main where

import           Algebra.Graph (deBruijn)
import           Genome
import           Graph



universalString :: Int -> String
universalString x = assemblePath $ head (eulerianPath $ deBruijn x "01")


main :: IO ()
main = do
    genome <- readFile "example.txt"
    putStrLn genome
    let assembly = silkAssemble (lines genome)
    putStrLn assembly
