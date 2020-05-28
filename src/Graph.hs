module Graph where

import           Algebra.Graph
import           Control.Monad
import           Data.List     (sort)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes)
import           Debug.Trace   (trace)


validPath :: Int -> [[a]] -> Bool
validPath bound x = isNull && maxLen <= bound
    where
        isNull = not (null x)
        maxLen = maximum (map length x)

consolidatePaths :: (Ord a, Eq a) => Int -> [[a]] -> [[a]]
consolidatePaths bound paths = concat $ takeWhile (validPath bound) (iterate cp paths)
    where
        cache = buildCache paths
        cp = consolidatePaths' cache

consolidatePaths' :: (Ord a, Eq a) => M.Map a [a] -> [[a]] -> [[a]]
consolidatePaths' cache toPaths = joinedPaths
    where
        joinedPaths = catMaybes $ map (consolidatePath' cache) toPaths

consolidatePath' :: (Ord a, Eq a) => M.Map a [a] -> [a] -> Maybe [a]
consolidatePath' cache path@(p:_) = (\x -> x ++ path) <$> (M.lookup p cache)

buildCache :: (Ord a, Eq a) => [[a]] -> M.Map a [a]
buildCache fromPaths =
    M.fromList $ map (\x -> case (reverse x) of (x:xs) -> (x, reverse xs)) fromPaths

isCycle :: Eq a => [a] -> Bool
isCycle path@(p:_) = p == last path

isLength :: Int -> [a] -> Bool
isLength x path = x == length path

cyclesOf :: Eq a => Int -> [[a]] -> [[a]]
cyclesOf len paths = filter (\x -> isCycle x && isLength len x) paths

pairs :: Ord a => [a] -> [(a, a)]
pairs []       = []
pairs x@(_:xs) = sort $ zip x xs

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
