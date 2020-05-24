module Lib (graph, findPaths, findPath, eulerian', tgraph, tedges) where

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


eulerian' :: Int -> [[Int]] -> [Int] -> [Int] -> [Int] -> [Int]
eulerian' _ _ _ [] circuit = reverse circuit
eulerian' v adj edge path@(p:ps) circuit
    | edge !! v /= 0 = eulerian' (last (adj !! v)) (popAtIndex v adj) (subAtIndex v edge) (v:path) circuit
    | otherwise = eulerian' p adj edge ps (v:circuit)

subAtIndex :: Int -> [Int] -> [Int]
subAtIndex v arr = map (\(i, x) -> if i == v then x - 1 else x) (zip [0..] arr)

popAtIndex :: Int -> [[Int]] -> [[Int]]
popAtIndex v adj = map (\(i, x) -> if i == v then init x else x) (zip [0..] adj)

tgraph :: [[Int]]
tgraph = [[1, 2, 3], [0, 2], [0, 1, 3, 3], [0, 2, 2]]

tedges :: [Int]
tedges = [3, 2, 4, 3]
