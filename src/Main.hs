{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Options.Applicative
import Data.String (fromString)
import Data.Text (pack, unpack)
import Control.Monad (liftM)
import Network.Socket (withSocketsDo)

import Bot
import Vindinium.Types
import Vindinium.Runner.ConsoleVindinium

data Cmd = Training Settings (Maybe Int) (Maybe Board)
         | Arena Settings
         deriving (Show, Eq)

cmdSettings :: Cmd -> Settings
cmdSettings (Training s _ _) = s
cmdSettings (Arena s) = s

settings :: Parser Settings
settings = Settings <$> (Key <$> argument (liftM pack str) (metavar "KEY"))
                    <*> (fromString <$> strOption (long "url" <> value "http://vindinium.org"))

trainingCmd :: Parser Cmd
trainingCmd = Training <$> settings
                       <*> optional (option auto (long "turns"))
                       <*> pure Nothing

arenaCmd :: Parser Cmd
arenaCmd = Arena <$> settings

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    s <- runConsoleVindinium (cmdSettings c) $
        case c of
            (Training _ t b) -> playTraining t b bot
            (Arena _)        -> playArena bot

    putStrLn $ "Game finished: " ++ unpack (stateViewUrl s)

main :: IO ()
main = withSocketsDo $
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
