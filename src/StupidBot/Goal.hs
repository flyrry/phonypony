module StupidBot.Goal where

import Vindinium.Types
import Utils
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S

data Mine = OwnedMine HeroId | AnyMine -- any excludes mines that we own
data Goal = Capture Mine | Kill HeroId | Heal | Survive

type GPS = State -> Goal

whereToGo :: GPS
whereToGo s
  | heroLife (stateHero s) < 25 = Heal -- very advanced heuristic ©
  | isTavernClose s && heroLife (stateHero s) < 90 = Heal
  | otherwise = Capture AnyMine

isGoal :: Goal -> State -> Pos -> Bool
isGoal goal s pos =
  let board = gameBoard $ stateGame s
      heroid = heroId $ stateHero s
      tile = tileAt board pos
  in if isNothing tile then False
     else case goal of
          Capture _ -> canCaptureMine (fromJust tile) heroid
          Kill hero -> heroid == hero
          Heal -> isTavern $ fromJust tile
          Survive -> isSafe board pos

canCaptureMine :: Tile -> HeroId -> Bool
canCaptureMine (MineTile owner) hero =
  case owner of
  Nothing -> True
  Just he -> he /= hero
canCaptureMine _ _ = False

isTavern :: Tile -> Bool
isTavern TavernTile = True
isTavern _ = False

isTavernClose :: State -> Bool
isTavernClose s =
  let board = gameBoard $ stateGame s
      pos = heroPos $ stateHero s
      adjacent = adjacentTiles board pos
  in foldl (\is p -> is || S.member p adjacent) False (taverns board)

isSafe :: Board -> Pos -> Bool
isSafe = undefined

