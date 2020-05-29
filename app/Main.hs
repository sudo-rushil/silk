module Main where

import           Algebra.Graph (deBruijn)
import           Graph



universalString :: Int -> String
universalString x = assemblePath $ head (eulerianPath $ deBruijn x "01")


main :: IO ()
main =
    putStrLn $ universalString 3
