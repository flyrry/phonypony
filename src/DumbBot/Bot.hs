module DumbBot.Bot (dumbBot) where

import DumbBot.Goal
import Vindinium.Types

import Debug.Trace

dumbBot :: Bot
dumbBot s = let goals = getGoals s
            in trace (show goals) $ Stay
