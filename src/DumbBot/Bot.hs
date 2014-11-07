module DumbBot.Bot (dumbBot) where

import Data.List (sortBy)
import Control.Monad (msum)

import DumbBot.Pathfinding
import DumbBot.Goal
import Vindinium.Types

dumbBot :: Bot
dumbBot state = let board = gameBoard $ stateGame state
                    hero = stateHero state
                    -- build board map which allows us to get a path to any
                    -- position on a board if it is possible to reach it
                    boardMap = buildBoardMap board hero
                    -- collect all goals on the board like taverns, mines
                    -- which do not belong to us and all enemies
                    goals = getGoals state
                    -- assign a value to each goal which indicates what
                    -- a potential profit (or loss) it might give us
                    score = map (goalScore boardMap) goals
                    -- calculate how far every goal is from hero's position
                    dist = map (goalDistance boardMap) goals
                    goalStats = zip3 goals score dist
                    -- sort goals first by the score (profit or loss) and
                    -- if it is the same (e.g. capturing any mine should
                    -- potentially give the same amount of profit) then we
                    -- select the closest one to our hero
                    goalValue (_, v1, d1) (_, v2, d2) = case compare v1 v2 of
                                                          EQ -> compare d1 d2
                                                          LT -> GT
                                                          GT -> LT
                    bestGoals = take 3 $ sortBy goalValue goalStats
                    -- we are currently working only with 3 best goals and
                    -- pick the one that it is possible to reach
                    -- and well, it might happen that all 3 of these goals
                    -- are unreachable; then we simply Stay in our place
                    -- TODO: maybe consider all goals which are profitable
                    bestAvailablePath = msum $ map (\(Goal _ pos, _, _) -> boardMap pos) bestGoals
                in case bestAvailablePath of
                     Nothing -> Stay
                     (Just path) -> walk hero path
