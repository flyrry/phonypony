module StupidBot.Bot (stupidBot) where

import Vindinium.Types
import Utils
import StupidBot.Goal

import qualified Data.Graph.AStar as AS
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Data.List (find)

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
      path = AS.aStar (adjacentTiles board)
                      (stepCost s goal)
                      (distanceEstimateTo goal s)
                      (isGoal goal s)
                      (heroPos hero)
  in fromMaybe [] path

distanceEstimateTo :: Goal -> State -> Pos -> Int
distanceEstimateTo Heal s pos =
  minimum $ map (manhattan pos) (taverns (gameBoard $ stateGame s))
distanceEstimateTo (Capture _) s pos =
  let board  = gameBoard $ stateGame s
  in minimum $ map (\p ->
      let heroid = heroId $ stateHero s
          Just m = tileAt board p
      in if canCaptureMine m heroid then manhattan pos p else 999) (mines board)
distanceEstimateTo (Kill hid) s pos =
  let heroes = gameHeroes $ stateGame s
      Just h = find (\e -> heroId e == hid) heroes
  in manhattan pos $ heroPos h
distanceEstimateTo _ _ _ = error "not implemented!"

stepCost :: State -> Goal -> Distance
stepCost s goal _ to =
  let board = gameBoard $ stateGame s
  in case fromJust $ tileAt board to of
     FreeTile -> case M.lookup to (enemyInfluence s) of
                  Nothing -> 1
                  Just x  -> x
     _ -> if isGoal goal s to then 1 else 999

enemyInfluence :: State -> M.Map Pos Int
enemyInfluence s =
  let enemies = gameHeroes $ stateGame s
      board   = gameBoard $ stateGame s
  in foldl (\m e -> heroInfluence m board e) M.empty enemies

heroInfluence :: M.Map Pos Int -> Board -> Hero -> M.Map Pos Int
heroInfluence mm board hero =
  foldl (\mo (dl, ps) -> foldl (\mi p ->
                          if inBoard board p then M.insert p dl mi
                          else mi) mo ps
        ) mm (influenceZone $ heroPos hero)

influenceZone :: Pos -> [(Int,[Pos])]
influenceZone (Pos x y) = [
  (8, [Pos (x-1) y, Pos (x+1) y, Pos x (y-1), Pos x (y+1)]),
  (7, [Pos (x+1) (y+1), Pos (x+1) (y-1), Pos (x-1) (y-1), Pos (x-1) (y+1)]),
  (3, [Pos (x+2) y, Pos (x-2) y, Pos x (y+2), Pos x (y-2)]),
  (2, [Pos (x+2) (y+1), Pos (x+2) (y-1), Pos (x+2) (y-2), Pos (x+2) (y+2),
       Pos (x+1) (y+2), Pos (x+1) (y-2), Pos (x-2) (y+1), Pos (x-2) (y-1),
       Pos (x-2) (y-2), Pos (x-2) (y+2), Pos (x-1) (y+2), Pos (x-1) (y-2)])
  ]

