module BFS where

import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graph as Graph

data BFS a = Struct [Graph.Vertex a] (Graph.Graph a)

step :: Ord a => BFS a -> BFS a
step (Struct l m) = maybe (Struct l m) (step . f) (List.uncons l)
    where
    f (v,vs) = if Map.member (Graph.label v) m then Struct vs m
        else Struct 
            (foldl (flip (:)) vs (Graph.children v)) 
            (Map.insert (Graph.label v) v m) 

scan :: Ord a => Graph.Vertex a -> Map.Map a (Graph.Vertex a)
scan v = m 
    where 
    (Struct _ m) = BFS.step $ Struct [v] Map.empty

connected :: Ord a => Graph.Graph a -> [Graph.Graph a]
connected = foldl f [] . Map.toList
    where
    f l (k,v) = if any (Map.member k) l then l else (BFS.scan v) : l

