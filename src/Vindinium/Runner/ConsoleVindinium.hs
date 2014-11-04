{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vindinium.Runner.ConsoleVindinium where

import Data.Aeson
import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Vindinium.Types
import Vindinium.Api

newtype ConsoleVindinium a = ConsoleVindinium (ReaderT Settings IO a)
                             deriving (Functor, Applicative, Monad, MonadIO)

instance Vindinium ConsoleVindinium where
    getSettings = ConsoleVindinium ask
    move s d = ConsoleVindinium $ do
      settings <- ask
      newState <- liftIO $ request settings (statePlayUrl s) (object [("dir", toJSON d)])
      return newState
    playTraining mt mb b = (ConsoleVindinium $ do
      settings <- ask
      initialState <- liftIO (startTraining settings mt mb)
      liftIO $ TIO.putStrLn $ T.append "View url: " (stateViewUrl initialState)
      return initialState) >>= playLoop b
    playArena b = (ConsoleVindinium $ do
      settings <- ask
      initialState <- liftIO (startArena settings)
      liftIO $ TIO.putStrLn "Got initial state from the server"
      liftIO $ TIO.putStrLn $ T.append "View url: " (stateViewUrl initialState)
      return initialState) >>= playLoop b
    outputState = undefined -- TODO: if needed

playLoop :: Vindinium m => Bot -> State -> m State
playLoop bot state =
    if (gameFinished . stateGame) state
        then return state
        else do
            newState <- move state (bot state)
            playLoop bot newState

runConsoleVindinium :: Settings -> ConsoleVindinium a -> IO a
runConsoleVindinium s (ConsoleVindinium v) = runReaderT v s
