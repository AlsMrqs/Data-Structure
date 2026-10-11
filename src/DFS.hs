module DFS where

import qualified Control.Monad.State as State
import qualified System.Random as Random
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Graph as Graph
import qualified Space as Space

data DFS a = Struct
    { database :: [(a,[a])]
    , visited  :: Map.Map a (Graph.Graph a) } 

instance Show a => Show (DFS a) where
    show (Struct db vstd) =
        "Database: "  ++ (show db) ++
        "\nVisited: " ++ (List.concat . map ((++) "\n" . show) $ Map.toList vstd)

remove :: Eq a => (a,[a]) -> DFS a -> DFS a
remove k _DFS = _DFS { database = List.delete k (database _DFS) }

insert :: Ord a => (a,Graph.Graph a) -> DFS a -> DFS a
insert k _DFS = _DFS { visited = Map.insert (fst k) (snd k) (visited _DFS) }

build :: Ord a => DFS a -> DFS a
build _DFS = case (List.uncons . database) _DFS of
    Nothing    -> _DFS
    Just (n,_) -> build . snd $ create n _DFS

create :: Ord a => (a,[a]) -> DFS a -> (Graph.Graph a, DFS a)
create k _DFS = (,) vertex newDFS
    where
    vertex         = Graph.Node (fst k) edge
    thunkDFS       = insert (fst k, vertex) $ remove k _DFS
    (newDFS, edge) = foldl (flip visit) (thunkDFS, []) (snd k)

visit :: Ord a => a -> (DFS a, [Graph.Graph a]) -> (DFS a, [Graph.Graph a])
visit x (_DFS, ls) = case (Map.lookup x . visited) _DFS of
    Just vtx -> (_DFS,   ls++[vtx])
    Nothing  -> (newDFS, ls++[newVtx])
        where
        (newVtx , newDFS) = flip create _DFS
            . maybe (x,[]) id 
            $ List.find ((==) x . fst) (database _DFS)

