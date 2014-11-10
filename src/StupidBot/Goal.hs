module StupidBot.Goal where

import Vindinium.Types
import Utils
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S
import Debug.Trace

data Mine = OwnedMine HeroId | AnyMine deriving (Show) -- any excludes mines that we own
data Goal = Capture Mine | Kill HeroId | Heal | Survive deriving (Show)

type GPS = State -> Goal

whereToGo :: GPS
whereToGo s
  | heroLife (stateHero s) < 20 = Heal -- very advanced heuristic ©
  | isTavernNearby s && heroLife (stateHero s) < 90 = Heal
  | amWinning s = Survive
  | otherwise = Capture AnyMine

amWinning :: State -> Bool
amWinning s =
  let enemies = getEnemies s
      me      = stateHero s
  in 2 * (heroMineCount me) > (maximum $ map heroMineCount enemies)


isGoal :: Goal -> State -> Pos -> Bool
isGoal goal s pos =
  let board = gameBoard $ stateGame s
      heroid = heroId $ stateHero s
      tile = tileAt board pos
  --in trace ("Goal: " ++ show goal) $
  in
     if isNothing tile then False
     else case goal of
          Capture _ -> canCaptureMine (fromJust tile) heroid
          Kill hero -> heroid == hero
          Heal -> isTavern $ fromJust tile
          Survive -> isSafe s pos

canCaptureMine :: Tile -> HeroId -> Bool
canCaptureMine (MineTile owner) hero =
  case owner of
  Nothing -> True
  Just he -> he /= hero
canCaptureMine _ _ = False

isTavern :: Tile -> Bool
isTavern TavernTile = True
isTavern _ = False

isSafe :: State -> Pos -> Bool
isSafe s pos = 0 == dangerLevelWithin 3 s (heroPos $ stateHero s) pos

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
