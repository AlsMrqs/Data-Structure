module Main (main) where

import qualified Control.Monad.State as State
import qualified System.Random as Random
import qualified Data.Set as Set
import qualified Data.Bool as Bool
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Generator as Generator
import qualified Graph as Graph
import qualified Space as Space
import qualified DFS as DFS
import qualified BFS as BFS

main :: IO ()
main = do
    putStrLn "Random data generated: "
    dataset <- Generator.generate 5
    mapM_ print dataset
    let graph = DFS.visited $ DFS.build (DFS.Struct dataset Map.empty)
    putStrLn "Graph.vert: "
    mapM_ (print . fst) (Map.toList graph)
    let node = fmap snd . List.take 1 $ Map.toList graph
        _BFS = BFS.scan $ BFS.Struct node Map.empty []
    putStrLn "BFS.mapped: "
    mapM_ print (BFS.mapped _BFS)
    return ()

