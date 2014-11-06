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
      path = AS.aStar (adjacentTiles board) (stepCost s goal) (distanceEstimateTo goal s) (isGoal goal s) (heroPos hero)
  in fromMaybe [] path

distanceEstimateTo :: Goal -> State -> Pos -> Int
distanceEstimateTo Heal s pos = minimum $ map (manhattan pos) (taverns (gameBoard $ stateGame s))
distanceEstimateTo (Capture _) s pos =
  let board  = gameBoard $ stateGame s
  in minimum $ map (\p ->
      let heroid = heroId $ stateHero s
          Just m = tileAt board p
      in if canCaptureMine m heroid then manhattan pos p else 999) (mines board)
distanceEstimateTo _ _ _ = error "not implemented!"

stepCost :: State -> Goal -> Pos -> Pos -> Int
stepCost s goal _ to =
  let board = gameBoard $ stateGame s
  in case tileAt board to of
      Nothing -> 999 -- or error?
      Just tile -> case tile of
                    FreeTile -> 1 -- TODO: consider adjacent heroes
                    _ -> if isGoal goal s to then 1 else 999

