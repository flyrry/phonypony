{-
module Bot
        ( bot
        )
    where
-}
module Bot where

import Vindinium

import qualified Data.Graph.AStar as AS
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

bot :: Bot
bot = stupidBot
--bot _ = Stay

stupidBot :: Bot
--stupidBot = undefined
stupidBot = directionTo whereToGo

whereToGo :: State -> Tile
whereToGo s
  | heroLife (stateHero s) < 25 = TavernTile -- very advanced heuristic ©
  | otherwise = MineTile Nothing

directionTo :: (State -> Tile) -> State -> Dir
directionTo gps s =
  let from = heroPos $ stateHero s
      path = shortestPathTo s $ gps s
  in case path of
      (p:_)  -> dirFromPos from p
      []     -> Stay

shortestPathTo :: State -> Tile -> [Pos]
shortestPathTo s goal =
  let board = gameBoard $ stateGame s
      hero = stateHero s
      path = AS.aStar (adjacentTiles board) (stepCost s goal) (distanceToClosest s goal) (isGoal goal s) (heroPos hero)
  in fromMaybe [] path

adjacentTiles :: Board -> Pos -> S.Set Pos
adjacentTiles board (Pos x y) =
  S.fromList $ foldl (\ps p -> if inBoard board p then p:ps else ps) [] [Pos x $ y+1, Pos x $ y-1, Pos (x+1) y, Pos (x-1) y]

isGoal :: Tile -> State -> Pos -> Bool
isGoal goal s pos =
  let board = gameBoard $ stateGame s
      heroid = heroId $ stateHero s
  in case tileAt board pos of
      Nothing -> False
      Just m@(MineTile _) -> not $ myMine heroid m
      Just t -> t == goal

distanceToClosest :: State -> Tile -> Pos -> Int
distanceToClosest s goal pos =
  let board = gameBoard $ stateGame s
      heroid = heroId $ stateHero s
  in case goal of
      TavernTile -> minimum $ map (distanceHeuristix pos) (taverns board)
      MineTile _ -> minimum $ map (\p ->
                    let Just m = tileAt board p
                    in if myMine heroid m then 999
                       else distanceHeuristix pos p) (mines board)
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

myMine :: HeroId -> Tile -> Bool
myMine hid (MineTile owner) =
  case owner of
    Nothing -> False
    Just oh -> hid == oh

stepCost :: State -> Tile -> Pos -> Pos -> Int
stepCost s goal _ to =
  let board = gameBoard $ stateGame s
      heroid = heroId $ stateHero s
  in case tileAt board to of
      Nothing -> 999 -- or error?
      Just tile -> case tile of
                    FreeTile -> 1 -- TODO: consider adjacent heroes
                    m@(MineTile _) -> if myMine heroid m then 999 else 0
                    t -> if t == goal then 0 else 999

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = x * boardSize b + y

