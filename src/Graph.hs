module Graph where

import           Algebra.Graph
import           Control.Monad
import           Data.List     (sort)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes)
import           Debug.Trace   (trace)


-- ***** Path consolidation functions *****

consolidatePaths :: (Ord a, Eq a) => Int -> [[a]] -> [[a]]
consolidatePaths bound paths = concat $ takeWhile (validPath bound) (iterate cp paths)
    where
        cache = buildCache paths
        cp = consolidatePaths' cache

validPath :: Int -> [[a]] -> Bool
validPath bound x = isNull && maxLen <= bound
    where
        isNull = not (null x)
        maxLen = maximum (map length x)

consolidatePaths' :: (Ord a, Eq a) => M.Map a [a] -> [[a]] -> [[a]]
consolidatePaths' cache toPaths = filter (not.null) joinedPaths
    where
        joinedPaths = concat $ map (consolidatePath' cache) toPaths

consolidatePath' :: (Ord a, Eq a) => M.Map a [a] -> [a] -> [[a]]
consolidatePath' cache path@(p:_) =
    case (M.lookup p cache) of
        Just x  -> map (\y -> y:path) x
        Nothing -> [[]]

buildCache :: (Ord a, Eq a) => [[a]] -> M.Map a [a]
buildCache fromPaths = foldr insertIntoCache M.empty fromPaths

insertIntoCache :: (Ord a, Eq a) => [a] -> M.Map a [a] -> M.Map a [a]
insertIntoCache path cache =
    case reverse path of
        (x:xs) ->
            case M.lookup x cache of
                Just ys -> M.insert x (ys ++ reverse xs) cache
                Nothing -> M.insert x (reverse xs) cache


-- ***** Eulerian Cycle Detection Functions *****

isCycle :: Eq a => [a] -> Bool
isCycle path@(p:_) = p == last path

isLength :: Int -> [a] -> Bool
isLength x path = x == length path

cyclesOf :: Eq a => Int -> [[a]] -> [[a]]
cyclesOf len paths = filter (\x -> isCycle x && isLength len x) paths

pairs :: Ord a => [a] -> [(a, a)]
pairs []       = []
pairs x@(_:xs) = sort $ zip x xs

-- | Generate all eulerian paths over a graph
eulerianPath :: (Eq a, Ord a) => Graph a -> [[a]]
eulerianPath graph = result
    where
        edges = edgeList graph
        len = length edges + 1
        paths = consolidatePaths len (map (\(a, b) -> [a, b]) edges)
        valid = cyclesOf len paths
        result = (filter (\x -> pairs x == edges) valid)

-- ***** Universal String Functions *****
-- | Assemble universal string from eulerian path
assemblePath :: [String] -> String
assemblePath (p:ps) = foldr (\x acc -> acc ++ [last x]) p (reverse ps)

-- Stop using paths with repeated edges
