module Vindinium.Play
        ( playTraining
        , playArena
        )
    where

import Control.Monad.IO.Class (liftIO)

import Vindinium.Types
import Vindinium.Api

playTraining :: Vindinium m => Maybe Int -> Maybe Board -> Bot -> m State
playTraining mt mb b = do
    settings <- getSettings
    liftIO( startTraining settings mt mb) >>= playLoop b

playArena :: Vindinium m => Bot -> m State
playArena b = do
    settings <- getSettings
    liftIO (startArena settings) >>= playLoop b

playLoop :: Vindinium m => Bot -> State -> m State
playLoop bot state =
    if (gameFinished . stateGame) state
        then return state
        else do
            newState <- move state (bot state)
            playLoop bot newState
