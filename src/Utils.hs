module Utils where

import Data.List (foldl', deleteBy)
import Data.Function (on)

import Vindinium.Types
import qualified Data.Set as S

numberOfHeroMines :: Board -> Hero -> Int
numberOfHeroMines board hero = foldl' mineTile 0 (boardTiles board)
  where
    mineTile acc (MineTile (Just heroId')) = if heroId' == heroId hero then acc + 1 else acc
    mineTile acc _ = acc

-- get all heroes except our own one
getEnemies :: State -> [Hero]
getEnemies s = let hero = stateHero s
               in deleteBy ((==) `on` heroId) hero (gameHeroes $ stateGame s)

isTavernNearby :: State -> Bool
isTavernNearby state =
    let board = gameBoard $ stateGame state
        pos = heroPos $ stateHero state
        neighbouringTiles = adjacentTiles board pos
    in foldl' (\result p -> result || S.member p neighbouringTiles) False (taverns board)

isEnemyNearby :: State -> Pos -> Bool
isEnemyNearby state pos =
    let board = gameBoard $ stateGame state
        enemies = getEnemies state
    in pos `S.member` S.unions (map (adjacentTiles board . heroPos) enemies)

adjacentTiles :: Board -> Pos -> S.Set Pos
adjacentTiles board (Pos x y) =
  S.fromList $ foldl (\ps p -> if inBoard board p then p:ps else ps) [] [Pos x $ y+1, Pos x $ y-1, Pos (x+1) y, Pos (x-1) y]

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = x * boardSize b + y

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

-- convert given board to a list of tiles with corresponding position
tilePosition :: Board -> [(Tile, Pos)]
tilePosition (Board size tiles _ _) = zip tiles [Pos x y | x <- [0..size-1], y <- [0..size-1]]

-- http://en.wikipedia.org/wiki/Taxicab_geometry
manhattan :: Distance
manhattan (Pos row1 col1) (Pos row2 col2) = abs (row1 - row2) + abs (col1 - col2)

-- vindinium has x/y axes flipped for some reason
dirFromPos :: Pos -> Pos -> Dir
dirFromPos (Pos fx fy) (Pos tx ty) =
  let x = tx - fx
      y = ty - fy
  in case (x, y) of
       (-1, 0) -> North
       (1, 0) -> South
       (0, -1) -> West
       (0, 1) -> East
       _ -> Stay
