{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vindinium.Runner.SDLVindinium where

import Data.Aeson
import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Vindinium.Types
import Vindinium.Api

newtype SDLVindinium a = SDLVindinium (ReaderT Settings IO a)
                         deriving (Functor, Applicative, Monad, MonadIO)

instance Vindinium SDLVindinium where
    getSettings = SDLVindinium ask
    move s d = SDLVindinium $ do
      settings <- ask
      newState <- liftIO $ request settings (statePlayUrl s) (object [("dir", toJSON d)])
      liftIO $ print (show newState)
      return newState

runSDLVindinium :: Settings -> SDLVindinium a -> IO a
runSDLVindinium s (SDLVindinium v) = runReaderT v s
