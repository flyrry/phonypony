module StupidBot.Bot (stupidBot) where

import Vindinium.Types
import Utils
import StupidBot.Goal

import qualified Data.Graph.AStar as AS
import Data.Maybe (fromMaybe)

stupidBot :: Bot
stupidBot = directionTo whereToGo

directionTo :: GPS -> State -> Dir
directionTo gps s =
  let from = heroPos $ stateHero s
      path = shortestPathTo s $ gps s
  in case path of
      (p:_)  -> dirFromPos from p
      []     -> Stay

shortestPathTo :: State -> Goal -> [Pos]
shortestPathTo s goal =
  let board = gameBoard $ stateGame s
      hero = stateHero s
      path = AS.aStar (adjacentTiles board) (stepCost s goal) (distanceToClosest s goal) (isGoal goal s) (heroPos hero)
  in fromMaybe [] path

distanceToClosest :: State -> Goal -> Pos -> Int
distanceToClosest s goal pos =
  let board = gameBoard $ stateGame s
      heroid = heroId $ stateHero s
  in case goal of
     Heal -> minimum $ map (distanceHeuristix pos) (taverns board)
     Capture _ -> minimum $ map (\p ->
                    let Just m = tileAt board p
                    in if canCaptureMine m heroid then distanceHeuristix pos p
                       else 999) (mines board)
     _ -> error "not implemented!"

distanceHeuristix :: Pos -> Pos -> Int -- just zigzag our way there!
distanceHeuristix (Pos fx fy) (Pos tx ty) =
  abs (fx - tx) + abs (fy - ty)

-- vindinium has x/y axes flipped for some reason
dirFromPos :: Pos -> Pos -> Dir
dirFromPos (Pos fx fy) (Pos tx ty) =
  let x = tx - fx
      y = ty - fy
  in case x of
      -1 -> North
      1  -> South
      0  -> case y of
            -1 -> West
            1  -> East
            0  -> Stay
            _  -> error "impossible to find direction!"
      _  -> error "impossible to find direction!"

stepCost :: State -> Goal -> Pos -> Pos -> Int
stepCost s goal _ to =
  let board = gameBoard $ stateGame s
  in case tileAt board to of
      Nothing -> 999 -- or error?
      Just tile -> case tile of
                    FreeTile -> 1 -- TODO: consider adjacent heroes
                    _ -> if isGoal goal s to then 1 else 999

