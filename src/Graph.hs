module Graph where

type Constellation a = [Star a]
type Star a = (a,[a])

data Graph a = Node 
    { vert :: a 
    , edge :: [Graph a] 
    } deriving Show

