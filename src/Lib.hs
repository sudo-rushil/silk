module Lib (graph, findPaths, findPath) where

import           Algebra.Graph (deBruijn, edgeList)
import           Data.List

type Node      = String
type Edge      = (Char, (String, String))
type Graph     = [Edge]
type Path      = String
type Candidate = (Path, Node, [Edge])

graph :: Graph
graph = map (\(s, e) -> (last e, (s, e))) . edgeList $ deBruijn 2 "01"


findPaths :: Graph -> [Node] -> [Path]
findPaths g ns = findPaths' g ns []

findPaths' :: Graph -> [Node] -> [Path] -> [Path]
findPaths' _ []     acc = acc
findPaths' g (n:ns) acc = findPaths' g ns acc'
    where acc' = findPath g n ++ acc

findPath :: Graph -> Node -> [Path]
findPath g node = findPath' [("", node, g)] []

findPath' :: [Candidate] -> [Path] -> [Path]
findPath' []                    acc = acc
findPath' ((path, _, []):xs)    acc = findPath' xs (path:acc)
findPath' ((path, node, es):xs) acc
    | null nextEdges = findPath' xs acc  -- dead-end, discard!
    | otherwise      = findPath' (xs' ++ xs) acc
    where nextEdges  = filter (\(_,(a, b)) -> a == node || b == node) es
          xs'        = nextPaths (path, node, es) nextEdges []

nextPaths :: Candidate -> [Edge] -> [Candidate] -> [Candidate]
nextPaths _                []     acc = acc
nextPaths (path, node, es) (x:xs) acc = nextPaths (path, node, es) xs acc'
    where acc'  = (path', node', delete x es ) : acc
          path' = path ++ [label]
          node' = if node == a then b else a
          (label, (a, b)) = x
