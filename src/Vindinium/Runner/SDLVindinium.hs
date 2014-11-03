{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vindinium.Runner.SDLVindinium where

import Data.Aeson
import Data.Word (Word32)
import Data.Bits ((.|.))
import Data.List (foldl')
import Data.Maybe (fromJust)
import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import qualified Data.Map.Lazy as M

import qualified Graphics.UI.SDL.Image as Image
import qualified Graphics.UI.SDL as SDL
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Storable (peek)

import Vindinium.Types
import Vindinium.Api
import Vindinium.Runner.Tileset

data SDLResources = SDLResources { _window :: SDL.Window
                                 , _renderer :: SDL.Renderer
                                 , _tileset :: M.Map String Sprite
                                 }

newtype SDLVindinium a = SDLVindinium (ReaderT (Settings, SDLResources) IO a)
                         deriving (Functor, Applicative, Monad, MonadIO)

instance Vindinium SDLVindinium where
    getSettings = SDLVindinium $ do
      (settings, _) <- ask
      return settings
    move s d = SDLVindinium $ do
      (settings, _) <- ask
      newState <- liftIO $ request settings (statePlayUrl s) (object [("dir", toJSON d)])
      liftIO $ print (show newState)
      return newState
    playTraining mt mb b = (SDLVindinium $ do
      (settings, sdlResources) <- ask
      initialState <- liftIO (startTraining settings mt mb)
      liftIO $ SDL.setWindowSize (_window sdlResources) (fromIntegral $ spriteSize * (boardSize . gameBoard . stateGame $ initialState)) (fromIntegral $ spriteSize * (boardSize . gameBoard . stateGame $ initialState))
      liftIO $ SDL.showWindow (_window sdlResources)
      -- TODO: draw initial state
      let tileset = _tileset sdlResources
          someSprite = fromJust $ M.lookup "rock" tileset
      _ <- liftIO $ renderSprite (_renderer sdlResources) someSprite 0 0
      liftIO $ SDL.renderPresent (_renderer sdlResources)
      return initialState) >>= playLoop b
    playArena b = (SDLVindinium $ do
      (settings, sdlResources) <- ask
      initialState <- liftIO (startArena settings)
      liftIO $ SDL.showWindow (_window sdlResources)
      -- TODO: draw initial state
      return initialState) >>= playLoop b

playLoop :: Vindinium m => Bot -> State -> m State
playLoop bot state =
    if (gameFinished . stateGame) state
        then return state
        else do
            newState <- move state (bot state)
            playLoop bot newState

handleNextEvent :: IO ()
handleNextEvent = do
    _ <- pollEvent
    return ()

runSDLVindinium :: Settings -> SDLVindinium a -> IO a
runSDLVindinium s (SDLVindinium v) = do
    initializeSDL [SDL.initFlagVideo] >>= catchRisky
    Image.imgInit [Image.InitPNG]
    window <- createWindow "Vindinium Viewer" >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated] >>= catchRisky
    _ <- clearWindow renderer
    tileset <- loadSprites renderer
    _ <- repeatedTimer handleNextEvent (usDelay 100)
    result <- runReaderT v (s, SDLResources window renderer tileset)
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Image.imgQuit
    SDL.quit
    return result

type Risky a = Either String a

data Colour = Black | White

windowWidth :: CInt
windowWidth = 640

windowHeight :: CInt
windowHeight = 480

initializeSDL :: [Word32] -> IO (Risky ())
initializeSDL flags = do
    initSuccess <- SDL.init $ foldl' (.|.) 0 flags
    return $ if initSuccess < 0 then Left "SDL could not initialize!" else Right ()

catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return

throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)

createWindow :: String -> IO (Risky SDL.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
  window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined windowWidth windowHeight SDL.windowFlagHidden
  return $ if window == nullPtr then Left "Window could not be created!" else Right window

createRenderer :: SDL.Window -> CInt -> [Word32] -> IO (Risky SDL.Renderer)
createRenderer window index flags = do
    renderer <- SDL.createRenderer window index $ foldl' (.|.) 0 flags
    return $ if renderer == nullPtr then Left "Renderer could not be created!" else Right renderer

clearWindow :: SDL.Renderer -> IO CInt
clearWindow renderer = do
    _ <- setColor renderer Black
    SDL.renderClear renderer

setColor :: SDL.Renderer -> Colour -> IO CInt
setColor renderer White = SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
setColor renderer Black = SDL.setRenderDrawColor renderer 0x00 0x00 0x00 0x00

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pointer -> do
  status <- SDL.pollEvent pointer
  if status == 1
    then maybePeek peek pointer
    else return Nothing
