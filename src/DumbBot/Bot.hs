module DumbBot.Bot (dumbBot) where

import DumbBot.Goal
import Vindinium.Types

import Debug.Trace

dumbBot :: Bot
dumbBot s = let goals = getGoals s
                -- calculate value of each goal
                -- sort them by biggest value
                -- pick the next goal that can be reached from given position
            in trace (show goals) $ Stay
