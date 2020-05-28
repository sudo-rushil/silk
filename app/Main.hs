module Main where

import           Graph
import           Lib

main :: IO ()
-- main = do
--     x <- getLine
--     let n = read x
main =
    putStrLn $ universalString 4
