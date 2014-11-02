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

bot :: Bot
bot = stupidBot
--bot _ = Stay

stupidBot :: Bot
--stupidBot = undefined
stupidBot state = shortestPathTo (heroPos $ stateHero state) (gameBoard $ stateGame state) (stateHero state) (MineTile Nothing)

shortestPathTo :: Pos -> Board -> Hero -> Tile -> Dir
shortestPathTo from board hero tile =
    let path = AS.aStar (adjacentTiles board) (passable board tile) (distanceToClosest tile board hero) (isTile tile board) from
    in case path of
        --Nothing -> []
        --Just x -> x
        Nothing -> Stay
        Just x -> case x of
                  [] -> Stay
                  (h:_) -> dirFromPos from h

adjacentTiles :: Board -> Pos -> S.Set Pos
adjacentTiles board (Pos x y) =
  S.fromList $ foldl (\ps p -> if inBoard board p then p:ps else ps) [] [Pos x $ y+1, Pos x $ y-1, Pos (x+1) y, Pos (x-1) y]

isTile :: Tile -> Board -> Pos -> Bool
isTile tile board pos = case tileAt board pos of
                        Nothing -> False
                        Just t -> t == tile

distanceToClosest :: Tile -> Board -> Hero -> Pos -> Int
distanceToClosest tile board hero pos =
  case tile of
    TavernTile -> minimum $ map (distanceHeuristix pos) (taverns board)
    MineTile _ -> minimum $ map (\p ->
                                  let (Just (MineTile owner)) = tileAt board p
                                  in case owner of
                                      Nothing -> distanceHeuristix pos p
                                      Just oh -> if heroId hero == oh then 999
                                                 else distanceHeuristix pos p) (mines board)
    _ -> error "not implemented!"

distanceHeuristix :: Pos -> Pos -> Int -- just zigzag our way there!
distanceHeuristix (Pos fx fy) (Pos tx ty) =
  (abs (fx - tx)) + (abs (fy - ty))

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

passable :: Board -> Tile -> Pos -> Pos -> Int
passable board dst (Pos fx fy) to@(Pos tx ty) =
  case tileAt board to of
    Nothing -> 999
    Just tile -> case tile of
                  FreeTile -> 1
                  t -> if t==dst then 0 else 999

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) = tileAt' b (Pos y x)

tileAt' :: Board -> Pos -> Maybe Tile
tileAt' b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = y * boardSize b + x

