module Utils where

import Vindinium.Types
import qualified Data.Set as S

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
  in case x of
      -1 -> North
      1  -> South
      0  -> case y of
            -1 -> West
            1  -> East
            0  -> Stay
            _  -> error "impossible to find direction!"
      _  -> error "impossible to find direction!"

