module Graph where

import           Algebra.Graph
import           Control.Monad
import           Data.List     (sort)
import           Debug.Trace   (trace)


-- ***** Path consolidation functions *****

validPath :: Int -> [[a]] -> Bool
validPath bound x = isNull && maxLen <= bound
    where
        isNull = not (null x)
        maxLen = maximum (map length x)

consolidatePaths :: Eq a => Int -> [[a]] -> [[a]]
consolidatePaths bound paths = concat $ takeWhile (validPath bound) (iterate cp paths)
    where
        cp = consolidatePaths' paths

consolidatePaths' :: Eq a => [[a]] -> [[a]] -> [[a]]
consolidatePaths' toPaths fromPaths = joinedPaths
    where joinedPaths = do
            p1 <- fromPaths
            (p2h:p2) <- toPaths
            guard (last p1 == p2h)
            return (p1 ++ p2)


-- ***** Eulerian Cycle Detection Functions *****

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

-- | Generate all eulerian paths over a graph
eulerianPath :: (Eq a, Ord a) => Graph a -> [[a]]
eulerianPath graph = result
    where
        edges = edgeList graph
        len = length edges + 1
        paths = consolidatePaths len (map (\(a, b) -> [a, b]) edges)
        valid = cyclesOf len paths
        result = (filter (\x -> pairs x == edges) valid)

-- | Assemble universal string from eulerian path
assemblePath :: [String] -> String
assemblePath (p:ps) = foldr (\x acc -> acc ++ [last x]) p (reverse ps)
