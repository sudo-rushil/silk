module Main where

import           Graph
import           Lib

main :: IO ()
main = do
    x <- getLine
    let n = read x
    putStrLn $ universalString n
