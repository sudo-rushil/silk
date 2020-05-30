module Main where

import           Algebra.Graph (deBruijn)
import           Genome
import           Graph



universalString :: Int -> String
universalString x = assemblePath $ head (eulerianPath $ deBruijn x "01")


main :: IO ()
-- main =
--     putStrLn $ universalString 3
main = do
    let assembly =  silkAssemble ["AAC","ACG","CGT","GTA","TAA"]
    putStrLn assembly
