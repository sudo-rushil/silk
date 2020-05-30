module Genome where

import           Algebra.Graph
import           Graph         (assemblePath, eulerianPath)


assembleGraph :: [String] -> Graph String
assembleGraph = edges . (map (\str -> (init str, tail str)))

silkAssemble :: [String] -> String
silkAssemble strs = circularize . assemblePath $ head (eulerianPath graph)
    where graph = assembleGraph strs

circularize :: String -> String
circularize assembly = take len assembly
    where
        isSame count (s:str) (r:revstr)
            | s == r = isSame (count + 1) str revstr
            | otherwise = count
        len = length assembly - isSame 0 assembly (reverse assembly)
