module BFS where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Graph as Graph

data BFS a = Struct
    { database :: [Graph.Graph a]
    , visited  :: Map.Map a (Graph.Graph a) 
    , mapped   :: [(a,[a])] 
    } deriving Show

pop :: BFS a -> BFS a 
pop _BFS = _BFS { database = List.drop 1 (database _BFS) }

push :: [Graph.Graph a] -> BFS a -> BFS a
push k _BFS = _BFS { database = List.foldl' (flip (:)) (database _BFS) k }

record :: Ord a => Graph.Graph a -> BFS a -> BFS a
record k _BFS = _BFS { mapped = star : (mapped _BFS) }
    where
    star = (,) (Graph.vert k) (map Graph.vert (Graph.edge k))

stamp :: Ord a => Graph.Graph a -> BFS a -> BFS a
stamp k _BFS = _BFS { visited = Map.insert (Graph.vert k) k (visited _BFS) }

visit :: Ord a => Graph.Graph a -> BFS a -> BFS a 
visit k = push (Graph.edge k) . pop . stamp k

isVisited :: Ord a => Graph.Graph a -> BFS a -> Bool
isVisited k _BFS = Map.member (Graph.vert k) (visited _BFS)

step :: Ord a => BFS a -> BFS a
step _BFS = case List.uncons (database _BFS) of
    Nothing    -> _BFS
    Just (v,_) -> if isVisited v _BFS
        then pop _BFS
        else record v $ visit v _BFS

scan :: Ord a => BFS a -> BFS a
scan _BFS = if null (database _BFS)
    then _BFS
    else scan (step _BFS)

