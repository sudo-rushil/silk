module Graph where

import           Algebra.Graph
import           Control.Monad
import           Data.List
import           Debug.Trace   (trace)


overlayPaths :: Show a => [[a]] -> [[a]] -> [[a]]
overlayPaths = (++)


connectPaths :: (Eq a, Show a) => [[a]] -> [[a]] -> [[a]]
connectPaths path1 path2 = connected
    where connected = do
            p1 <- path1
            p2 <- path2
            return (p1 ++ p2)


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


-- | Generate path from a graph.
-- Needs more work... doesn't yet handle self-adjacency
path :: (Eq a, Show a) => Graph a -> [[a]]
-- path graph = foldg [[]] (\x -> trace ("Vertex: " ++ show [[x]]) [[x]]) overlayPaths connectPaths graph
path = foldg [[]] (pure.pure) overlayPaths connectPaths

isCycle :: Eq a => [a] -> Bool
isCycle path@(p:_) = p == last path
