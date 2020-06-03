{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString                  as B
import           Fasta                            (Fasta, fastaParser,
                                                   pullSequence)
import           Genome                           (makeKmers, silkAssemble)
import           System.Environment               (getArgs)


-- First option: kmers
-- Second option: filepath
main :: IO ()
main = do
    (k:fpath:_) <- getArgs
    file <- B.readFile fpath
    let fasta = getFasta file
    let genome = makeKmers (read k) (pullSequence fasta)
    putStrLn $ silkAssemble genome

getFasta :: B.ByteString -> Fasta
getFasta str =
    case parseOnly fastaParser str of
        (Left _)  -> error "parsing failed"
        (Right f) -> f
