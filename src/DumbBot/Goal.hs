module DumbBot.Goal where

import Data.List (deleteBy)
import Data.Function (on)

import DumbBot.Pathfinding
import Vindinium.Types
import Utils

data Action = Heal | Kill Hero | CaptureMine deriving (Show)

data Goal = Goal Action Pos deriving (Show)

goalScore :: BoardMap -> Goal -> Int
goalScore = undefined

-- if there is currently no path to desired destination
-- then return max possible distance otherwise just return
-- the actual distance to the given goal position
goalDistance :: BoardMap -> Goal -> Int
goalDistance boardMap (Goal _ pos) =
    let path = boardMap pos
    in maybe maxBound distance path

-- collect all possible goals from the board that we might be interested in pursuing
getGoals :: State -> [Goal]
getGoals s = let enemies = getEnemies s
                 attackableMines = getMines s
                 allTaverns = taverns $ gameBoard $ stateGame s
             in concat [ map (\enemy -> Goal (Kill enemy) (heroPos enemy)) enemies
                       , map (Goal CaptureMine) attackableMines
                       , map (Goal Heal) allTaverns
                       ]

-- get all heroes except our own one
getEnemies :: State -> [Hero]
getEnemies s = let hero = stateHero s
               in deleteBy ((==) `on` heroId) hero (gameHeroes $ stateGame s)

-- get all mines except the ones we own
getMines :: State -> [Pos]
getMines s = let hero = stateHero s
                 board = tilePosition $ gameBoard $ stateGame s
                 attackableMineTiles (MineTile Nothing) = True
                 attackableMineTiles (MineTile (Just heroId')) = heroId' /= heroId hero
                 attackableMineTiles _ = False
             in map snd $ filter (attackableMineTiles . fst) board
