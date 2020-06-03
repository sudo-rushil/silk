{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Fasta where


import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as B
import           Data.ByteString.Char8            (pack, unpack)



-- Parser Types

type Header = [B.ByteString]
type Sequence = B.ByteString

data Record = Record
    { recHeaders  :: Header
    , recSequence :: Sequence
    }
    deriving Show

type Fasta = [Record]


-- Parsers

isLineEnd :: Char -> Bool
isLineEnd c = c == '\r' || c == '\n'

fastaParser :: Parser Fasta
fastaParser = many $ recordParser

headerParser :: Parser Header
headerParser = do
    char '>'
    header <- takeTill isLineEnd
    return $ [header]

sequenceParser :: Parser Sequence
sequenceParser = takeTill isLineEnd

recordParser :: Parser Record
recordParser = do
    header <- headerParser
    endOfLine
    sequence <- sequenceParser
    endOfLine
    return $ Record header sequence


-- Operations

pullSequence :: Fasta -> String
pullSequence = concat . (map (unpack . recSequence))
