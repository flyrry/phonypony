module DumbBot.Goal where

import DumbBot.Pathfinding
import DumbBot.PotentialScore
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

needToHeal :: Hero -> Int -> Bool
needToHeal hero dist = fromIntegral (heroLife hero) <= max (90 - dist * 5) 10

tooMuchHealth :: Hero -> Bool
tooMuchHealth hero = heroLife hero >= 90

gold :: (RealFrac a, Num a) => a -> Int
gold = floor . (*10)

goalScore :: State -> BoardMap -> Goal -> Int
goalScore state boardMap (Goal action pos) =
    let turn = fromIntegral $ gameTurn $ stateGame state
        maxTurn = fromIntegral $ gameMaxTurns $ stateGame state
        calculateScore path =
          let dist = distance path
              potentialGoalScore = scoreAction state dist action
          in potentialScore potentialGoalScore turn (min 200 (maxTurn - turn))
    in maybe (-9999) calculateScore (boardMap pos)

scoreAction :: State -> Int -> Action -> PotentialScore Int
scoreAction state dist CaptureMine =
    let ourHero = stateHero state
        turn = fromIntegral $ gameTurn $ stateGame state
    in if canCaptureMine ourHero dist
         then timespan (gold (0.1::Double)) (turn + dist) (constant (gold (1::Double)))
         else loseEverything ourHero

scoreAction state dist Heal =
    let ourHero = stateHero state
    in case () of
         _
          | needToHeal ourHero dist -> negative (loseEverything ourHero)
          | tooMuchHealth ourHero -> loseEverything ourHero
          | dist > 1 -> constant (gold (0.1::Double))
          | otherwise -> value (gold (-2::Double))

scoreAction state dist (Kill enemy) =
    let ourHero = stateHero state
    in if dist < 7 && canKill ourHero enemy dist
         then negative (loseEverything enemy)
         else loseEverything ourHero

loseEverything :: Hero -> PotentialScore Int
loseEverything hero = constant (negate (gold (fromIntegral $ heroMineCount hero :: Double)))

-- if there is currently no path to desired destination
-- then return max possible distance otherwise just return
-- the actual distance to the given goal position
goalDistance :: BoardMap -> Goal -> Int
goalDistance boardMap (Goal _ pos) =
    let path = boardMap pos
    in maybe 9999 distance path

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
