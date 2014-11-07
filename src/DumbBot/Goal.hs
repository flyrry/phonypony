module DumbBot.Goal where

import DumbBot.Pathfinding
import Vindinium.Types
import Utils

data Action = Heal | Kill Hero | CaptureMine deriving (Show)

data Goal = Goal Action Pos deriving (Show)

canCaptureMine :: Hero -> Int -> Bool
canCaptureMine ourHero dist = fromIntegral (heroLife ourHero) - dist > 20

canKill :: Hero -> Hero -> Int -> Bool
canKill ourHero enemy dist = 
    let ourHeroLife = fromIntegral $ heroLife ourHero
        enemyLife = fromIntegral $ heroLife enemy
    in ourHeroLife - dist > enemyLife + 20

goalScore :: State -> BoardMap -> Goal -> Int
goalScore state boardMap (Goal action pos) =
    let ourHero = stateHero state
        ourHeroHealth = heroLife ourHero
        turn = fromIntegral $ gameTurn $ stateGame state
        maxTurn = fromIntegral $ gameMaxTurns $ stateGame state
        actionScore path =
          let dist = distance path -- how many turns we need to reach the goal
          in case action of
               CaptureMine -> if canCaptureMine ourHero dist     -- will we have enough life when we reach the mine?
                                then (maxTurn - turn - dist) * 1 -- 1 gold per turn from the mine after acquiring it
                                else minBound                    -- no way, let's not suicide
               Heal -> if ourHeroHealth < 21
                         then maxBound -- healing is highest priority when our health is low
                         else if isTavernNearby state && ourHeroHealth < 90 -- we are near the Tavern
                                then maxBound                               -- so how about we heal to full
                                else minBound                               -- no need as we are pretty high on health
               (Kill enemy) -> if dist < 7 && canKill ourHero enemy dist
                                 then (maxTurn - turn - dist) * numberOfHeroMines (gameBoard $ stateGame state) enemy
                                 else minBound -- abandon ship!
    in maybe minBound actionScore (boardMap pos)

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

-- get all mines except the ones we own
getMines :: State -> [Pos]
getMines s = let hero = stateHero s
                 board = tilePosition $ gameBoard $ stateGame s
                 attackableMineTiles (MineTile Nothing) = True
                 attackableMineTiles (MineTile (Just heroId')) = heroId' /= heroId hero
                 attackableMineTiles _ = False
             in map snd $ filter (attackableMineTiles . fst) board
