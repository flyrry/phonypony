module DumbBot.Bot (dumbBot) where

import Data.List (sortBy)
import Control.Monad (msum)

import DumbBot.Pathfinding
import DumbBot.Goal
import Vindinium.Types

dumbBot :: Bot
dumbBot state = let board = gameBoard $ stateGame state
                    hero = stateHero state
                    boardMap = buildBoardMap board hero
                    goals = getGoals state
                    score = map (goalScore boardMap) goals
                    dist = map (goalDistance boardMap) goals
                    goalStats = zip3 goals score dist
                    goalValue (_, v1, d1) (_, v2, d2) = case compare v1 v2 of
                                                          EQ -> compare d1 d2
                                                          LT -> GT
                                                          GT -> LT
                    bestGoals = take 3 $ sortBy goalValue goalStats
                    bestAvailablePath = msum $ map (\(Goal _ pos, _, _) -> boardMap pos) bestGoals
                in case bestAvailablePath of
                     Nothing -> Stay
                     (Just path) -> walk hero path
