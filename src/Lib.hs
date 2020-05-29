module Lib where

import           Algebra.Graph (deBruijn, edgeList)
import           Data.List

-- import qualified Graph
import           Graph

universalString :: Int -> String
universalString x = assemblePath $ head (eulerianPath $ deBruijn x "01")
