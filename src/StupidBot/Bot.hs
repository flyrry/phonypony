module StupidBot.Bot (stupidBot) where

import Vindinium.Types
import Utils
import StupidBot.Goal

import qualified Data.Graph.AStar as AS
import qualified Data.Set as S
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
stepCost s goal from to =
  let board = gameBoard $ stateGame s
  in case fromJust $ tileAt board to of
     FreeTile -> dangerLevelWithin 3 s from to
     _ -> if isGoal goal s to then 1 else 999

dangerLevelWithin :: Int -> State -> Pos -> Pos -> Int
dangerLevelWithin 0 _ _ _ = 0
dangerLevelWithin steps s from pos =
  let next = S.delete from $ adjacentTiles (gameBoard $ stateGame s) pos
      heroes = heroesNearby s pos
  in if null heroes then sum $ map (dangerLevelWithin (steps-1) s pos) (S.toList next)
     else sum $ map (\_ -> calcCost steps) heroes

calcCost :: Int -> Int
calcCost steps = round $ (1 / (toRational steps)) * 8

heroesNearby :: State -> Pos -> [Hero]
heroesNearby s pos =
  foldl (\es e ->
      if pos `S.member` (adjacentTiles (gameBoard $ stateGame s) (heroPos e))
      then e:es else es) [] (getEnemies s)
