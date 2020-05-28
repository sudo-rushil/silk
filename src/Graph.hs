module Graph where

import           Algebra.Graph
import           Control.Monad
import           Data.List     (sort)
import           Debug.Trace   (trace)


consolidatePaths :: Eq a => Int -> [[a]] -> [[a]]
consolidatePaths bound paths = concat $ takeWhile (\x -> not (null x) && (maximum $ map length x) <= bound) (iterate cp paths)
    where
        cp = consolidatePaths' paths


consolidatePaths' :: Eq a => [[a]] -> [[a]] -> [[a]]
consolidatePaths' toPaths fromPaths = joinedPaths
    where joinedPaths = do
            p1 <- fromPaths
            (p2h:p2) <- toPaths
            guard (last p1 == p2h)
            return (p1 ++ p2)

isCycle :: Eq a => [a] -> Bool
isCycle path@(p:_) = p == last path

isLength :: Int -> [a] -> Bool
isLength x path = x == length path

cyclesOf :: Eq a => Int -> [[a]] -> [[a]]
cyclesOf len paths = filter (\x -> isCycle x && isLength len x) paths

pairs :: Ord a => [a] -> [(a, a)]
pairs [] = []
pairs xs = sort $ zip xs (tail xs)

g = deBruijn 2 "01"

eulerianPath :: (Eq a, Ord a) => Graph a -> [[a]]
eulerianPath graph = result
    where
        edges = edgeList graph
        len = length edges + 1
        paths = consolidatePaths len (map (\(a, b) -> [a, b]) edges)
        valid = cyclesOf len paths
        result = (filter (\x -> pairs x == edges) valid)
