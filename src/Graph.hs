module Graph where

import           Algebra.Graph
import           Control.Monad
import           Data.List     (intersect, isSubsequenceOf, sort)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes)
import qualified Data.Set      as S
import           Debug.Trace   (trace)

-- Store paths in a set
-- ***** Path consolidation functions *****

onlyUnique :: Ord a => [[a]] -> [[a]]
onlyUnique = S.toList . S.fromList


consolidatePaths :: (Ord a, Eq a) => [(a, a)] -> Int -> [[a]] -> [[a]]
consolidatePaths edges bound paths = concat $ takeWhile (validPath bound) (iterate cp paths)
    where
        cache = buildCache paths
        cp = consolidatePaths' edges cache


validPath :: Int -> [[a]] -> Bool
validPath bound path = isNull && maxLen <= bound
    where
        isNull = not (null path)
        maxLen = maximum (map length path)


-- | Return false if path uses an edge twice.
repeatedEdges :: (Ord a, Eq a) => [(a, a)] -> [a] -> Bool
repeatedEdges edges path = isSubsequenceOf pathEdges sortedEdges
    where
        pathEdges = pairs path
        sortedEdges = sort edges


consolidatePaths' :: (Ord a, Eq a) => [(a, a)] -> M.Map a [a] -> [[a]] -> [[a]]
consolidatePaths' edges cache toPaths = (onlyUnique . filter valid) joinedPaths
    where
        joinedPaths = concat $ map (consolidatePath' cache) toPaths
        valid path = not (null path) && repeatedEdges edges path


consolidatePath' :: (Ord a, Eq a) => M.Map a [a] -> [a] -> [[a]]--CHANGE
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
        paths = consolidatePaths edges len (map (\(a, b) -> [a, b]) edges)
        valid = cyclesOf len paths
        result = (filter (\x -> pairs x == edges) valid)


-- ***** Universal String Functions *****
-- | Assemble universal string from eulerian path
assemblePath :: [String] -> String
assemblePath (p:ps) = foldr (\x acc -> acc ++ [last x]) p (reverse ps)
