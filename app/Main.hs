{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Algebra.Graph                    (deBruijn)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as B
import           Fasta
import           Genome
import           Graph


universalString :: Int -> String
universalString x = assemblePath $ head (eulerianPath $ deBruijn x "01")

testParse :: IO ()
testParse = B.readFile "example.fa" >>= print . parseOnly fastaParser

main :: IO ()
main = do
    genome <- readFile "example.txt"
    putStrLn genome
    let assembly = silkAssemble (lines genome)
    putStrLn assembly
