module Vertex where

import qualified Data.Map as Map
import qualified System.Random as Random

data Vertex a = Node a [Vertex a]
    deriving (Show)

create :: Eq a => (a, [a]) -> Map.Map [Int] (Vertex a)
create _ = Map.empty

