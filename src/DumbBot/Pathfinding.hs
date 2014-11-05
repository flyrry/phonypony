module DumbBot.Pathfinding where

import Data.List (unfoldr, foldl')
import Data.PSQueue (PSQ, Binding(..))
import Control.Applicative ((<$>))
import Control.Monad (unless, forM_)
import Control.Monad.State (execState, get, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.PSQueue as PSQ

import Vindinium.Types

-- Based on https://gist.github.com/kazu-yamamoto/5218431
--          http://mew.org/~kazu/material/2012-psq.pdf

type Graph = Pos -> [Pos]
type Cost = Int
type Vertex = Pos
type Queue = PSQ Vertex Priority
type Mapping = (Vertex, Priority)
type Distance = Vertex -> Vertex -> Cost

data Dijkstra = Dijkstra { distance :: M.Map Vertex Cost
                         , previous :: M.Map Vertex Vertex
                         }

data Priority = Priority Cost Vertex deriving (Eq)

instance Ord Priority where
    Priority cost1 _ `compare` Priority cost2 _ = cost1 `compare` cost2

adjacent :: Graph -> Vertex -> [(Vertex, Cost)]
adjacent = undefined

-- given a start position construct shortest paths to all other positions
dijkstra :: Graph -> Distance -> Vertex -> Dijkstra
dijkstra graph dist start = buildDijkstra $ unfoldr (step graph dist) $ relax (start, Priority 0 start) queue
  where
    queue = PSQ.fromList $ map (\v -> v :-> Priority maxBound start) (S.toList $ vertices graph start)
    buildDijkstra = foldl' insertEdge (Dijkstra M.empty M.empty)
    insertEdge (Dijkstra d p) (destination, cost, source) = Dijkstra (M.insert destination cost d) (M.insert destination source p)

-- construct next edge in the graph taking the vertex with minimal cost
step :: Graph -> Distance -> Queue -> Maybe ((Vertex, Cost, Vertex), Queue)
step graph dist queue = extractMin graph dist <$> PSQ.minView queue

-- grab an item from PSQueue with minimal priority
-- and update priority of its neighbours
extractMin :: Graph -> Distance -> (Binding Vertex Priority, Queue) -> ((Vertex, Cost, Vertex), Queue)
extractMin graph dist (destination :-> Priority cost source, queue) = ((destination, cost, source), relaxList queue adj)
  where
    adj = [(neighbour, Priority (cost + dist destination neighbour) destination) | neighbour <- graph destination]

-- update priority of a given key in PSQueue
relax :: Mapping -> Queue -> Queue
relax (key, priority@(Priority cost _)) = PSQ.adjust update key
  where
    update otherPriority@(Priority otherCost _)
      | cost < otherCost = priority
      | otherwise = otherPriority

-- update priorities of given keys in PSQueue
relaxList :: Queue -> [Mapping] -> Queue
relaxList = foldr relax

-- collect all unique vertices from graph
vertices :: Graph -> Vertex -> S.Set Vertex
vertices graph start = execState (visitFrom start) S.empty
  where
    visitFrom start' =
      forM_ (graph start') $ \neighbour ->
        get >>= \v -> unless (neighbour `S.member` v) $ modify (S.insert neighbour) >> visitFrom neighbour

-- distance to a goal position
distanceDijkstra :: Dijkstra -> Vertex -> Maybe Cost
distanceDijkstra (Dijkstra dist _) goal = M.lookup goal dist

-- path to a goal position
pathDijkstra :: Dijkstra -> Vertex -> Maybe [Vertex]
pathDijkstra (Dijkstra _ prev) goal =
    case searchBackwards goal of
      [_] -> Nothing
      path -> Just (reverse path)
    where
      searchBackwards goal' = goal' : maybe [] searchBackwards (M.lookup goal' prev)
