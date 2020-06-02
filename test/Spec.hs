module Main where

import           Algebra.Graph (deBruijn, edgeList)
import           Genome
import           Graph
import           Test.Hspec

exampleEdges = [[0, 1], [1, 2], [2, 3], [4, 5], [5, 6]]

exampleGraph = deBruijn 2 "01"

examplePath = ["00", "00", "01", "10", "00"]


main :: IO ()
main = hspec $ do
    describe "Path consolidation functions" $ do
        it "Case 1: [[0, 1], [1, 2], [2, 3], [4, 5], [5, 6]]" $
            consolidatePaths (map (\(a:b:_) -> (a, b)) exampleEdges) 4 exampleEdges == (exampleEdges ++ [[0, 1, 2], [1, 2, 3], [4, 5, 6], [0, 1, 2, 3]])

        describe "repeatedEdges" $ do
            it "Positive Case" $
                repeatedEdges [(0, 0), (0, 1), (1, 2), (2, 3)] [0, 0, 1, 2, 3]

            it "Negative Case" $
                not $ repeatedEdges [(0, 0), (0, 1), (1, 2), (2, 3)] [0, 0, 0, 1, 2, 3]

    describe "Eulerian Cycle Detection Functions" $ do
        describe "isCycle" $ do
            it "Positive Case" $
                isCycle examplePath

            it "Negative Case" $
                not (isCycle ["00", "00", "01", "10", "01"])

        describe "isLength" $ do
            it "Positive Case" $
                isLength 5 examplePath

            it "Negative Case" $
                not (isLength 6 examplePath)

        describe "eulerianPath" $ do
            it "returned path is right length" $
                case eulerianPath exampleGraph of
                    (p:_) -> (length p) == 1 + length (edgeList exampleGraph)

    describe "Genome Assembly Functions" $ do
        describe "makeKmers" $ do
            it "3-mer Case" $
                makeKmers 3 "ATGCCATGGG" == ["GAT","GGA","GGG","TGG","ATG","CAT","CCA","GCC","TGC","ATG"]
