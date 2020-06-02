module Genome where

import           Graph (eulerianPathFromEdges)


assembleGraph :: [String] -> [(String, String)]
assembleGraph = map (\str -> (init str, tail str))


silkAssemble :: [String] -> String
silkAssemble strs = circularize . assemblePath $ head (eulerianPathFromEdges graph)
    where graph = assembleGraph strs


circularize :: String -> String
circularize assembly = take len assembly
    where
        totalLen = length assembly
        isSame count str revstr
            | count > totalLen = totalLen
            | isCircular count str revstr = count
            | otherwise = isSame (count + 1) str revstr
        len = totalLen - isSame 1 assembly (reverse assembly)


isCircular :: Int -> String -> String -> Bool
isCircular len str revstr = back revstr == (take len str)
    where back = reverse . (take len)


assemblePath :: [String] -> String
assemblePath (p:ps) = foldr (\x acc -> acc ++ [last x]) p (reverse ps)


makeKmers :: Int -> String -> [String]
makeKmers k str = kmers' (str ++ str) []
    where
        kmers' str'@(_:remStr) acc
            | length acc >= length str = acc
            | otherwise = kmers' remStr (take k str' : acc)
