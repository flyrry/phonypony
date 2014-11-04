{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vindinium.Runner.SDLVindinium where

import Data.Aeson
import Data.Word (Word32)
import Data.Bits ((.|.))
import Data.List (foldl', sortBy)
import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)
import Control.Applicative (Applicative)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ST (ST, RealWorld, stToIO)
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted

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

data Resources s = Resources { _settings :: Settings
                             , _window :: SDL.Window
                             , _renderer :: SDL.Renderer
                             , _tileset :: Tileset
                             , _background :: STRef s [Sprite]
                             }

newtype SDLVindinium a = SDLVindinium (ReaderT (Resources RealWorld) IO a)
                         deriving (Functor, Applicative, Monad, MonadIO)

instance Vindinium SDLVindinium where
    getSettings = SDLVindinium $ do
      resources <- ask
      return (_settings resources)
    move s d = SDLVindinium $ do
      resources <- ask
      liftIO $ request (_settings resources) (statePlayUrl s) (object [("dir", toJSON d)])
    playTraining mt mb b = (SDLVindinium $ do
      resources <- ask
      initialState <- liftIO (startTraining (_settings resources) mt mb)
      let board = gameBoard . stateGame $ initialState
          background = generateBackground (_tileset resources) board
      liftIO $ stToIO $ writeSTRef (_background resources) background
      liftIO $ SDL.setWindowSize (_window resources) (fromIntegral $ spriteSize * boardSize board) (fromIntegral $ spriteSize * boardSize board)
      liftIO $ SDL.showWindow (_window resources)
      return initialState) >>= playLoop b
    playArena b = (SDLVindinium $ do
      resources <- ask
      initialState <- liftIO (startArena $ _settings resources)
      let board = gameBoard . stateGame $ initialState
          background = generateBackground (_tileset resources) board
      liftIO $ stToIO $ writeSTRef (_background resources) background
      liftIO $ SDL.setWindowSize (_window resources) (fromIntegral $ spriteSize * boardSize board) (fromIntegral $ spriteSize * boardSize board)
      liftIO $ SDL.showWindow (_window resources)
      return initialState) >>= playLoop b
    outputState s = SDLVindinium $ do
      resources <- ask
      background <- liftIO $ stToIO $ readSTRef $ _background resources
      liftIO $ drawBoard (_renderer resources) (_tileset resources) background (stateGame s)
      liftIO $ SDL.renderPresent (_renderer resources)

playLoop :: Vindinium m => Bot -> State -> m State
playLoop bot state = do
    -- liftIO $ print state
    outputState state
    if (gameFinished . stateGame) state
        then return state
        else do
            newState <- move state (bot state)
            playLoop bot newState

initResources :: Settings -> SDL.Window -> SDL.Renderer -> Tileset -> ST s (Resources s)
initResources s w r t = do
    b <- newSTRef []
    return Resources { _settings = s
                     , _window = w
                     , _renderer = r
                     , _tileset = t
                     , _background = b
                     }

generateBackground :: Tileset -> Board -> [Sprite]
generateBackground tileset board = map tileToSprite (boardTiles board)
  where
    -- TODO: generate a better background with border, water and stuff
    tileToSprite _ = getSprite tileset "plain"

drawBoard :: SDL.Renderer -> Tileset -> [Sprite] -> Game -> IO ()
drawBoard renderer tileset background game = do
    let board = gameBoard game
        heroes = gameHeroes game
    -- print $ show heroes
    drawBackground renderer (boardSize board) background
    drawWoodTiles renderer tileset (boardSize board) (boardTiles board)
    drawSpawningTiles renderer tileset heroes
    drawTavernTiles renderer tileset (taverns board)
    -- TODO: merge goblin + mines into one function
    drawGoblinTiles renderer tileset (boardSize board) (boardTiles board)
    drawMineTiles renderer tileset (boardSize board) (boardTiles board)
    drawHeroTiles renderer tileset heroes

drawBackground :: SDL.Renderer -> Int -> [Sprite] -> IO ()
drawBackground renderer size background =
    mapM_ (\(i, sprite) -> renderAtPos renderer sprite (indexToPos i size)) (zip [0..] background)

drawWoodTiles :: SDL.Renderer -> Tileset -> Int -> [Tile] -> IO ()
drawWoodTiles renderer tileset size tiles = do
    -- TODO: blend mode should be set when we load texture
    setSpriteBlendMode woodTile SDL.blendModeBlend
    mapM_ (\(i, tile) -> renderTileAtPos tile (indexToPos i size)) (zip [0..] tiles)
    where
      woodTile = getSprite tileset "tree"
      renderTileAtPos WoodTile pos = renderAtPos renderer woodTile pos
      renderTileAtPos _ _ = return ()

drawSpawningTiles :: SDL.Renderer -> Tileset -> [Hero] -> IO ()
drawSpawningTiles renderer tileset heroes = do
    -- TODO: blend mode should be set when we load texture
    setSpriteBlendMode rez1 SDL.blendModeBlend
    setSpriteBlendMode rez2 SDL.blendModeBlend
    setSpriteBlendMode rez3 SDL.blendModeBlend
    setSpriteBlendMode rez4 SDL.blendModeBlend
    mapM_ (\hero -> renderAtPos renderer (getHeroRezMark $ heroId hero) (heroPosToDraw $ heroSpawnPos hero)) heroes
    where
      rez1 = getSprite tileset "rez1"
      rez2 = getSprite tileset "rez2"
      rez3 = getSprite tileset "rez3"
      rez4 = getSprite tileset "rez4"
      heroPosToDraw Pos {posX = x, posY = y} = Pos (x * spriteSize - 4) (y * spriteSize - 8)
      -- TODO: this begs for refactoring
      getHeroRezMark (HeroId x) = case x of
                                    1 -> rez1
                                    2 -> rez2
                                    3 -> rez3
                                    4 -> rez4
                                    _ -> error $ "Unknown hero id: " ++ show x

drawTavernTiles :: SDL.Renderer -> Tileset -> [Pos] -> IO ()
drawTavernTiles renderer tileset pos = do
    let tavernPositions = sortBy (\Pos {posX = _, posY = y1} Pos {posX = _, posY = y2} -> if y1 < y2 then LT else GT) pos
    -- TODO: blend mode should be set when we load texture
    setSpriteBlendMode tavern SDL.blendModeBlend
    mapM_ (renderAtPos renderer tavern . tavernPosToDraw) tavernPositions
    where
      tavern = getSprite tileset "tavern"
      tavernPosToDraw Pos {posX = x, posY = y} = Pos (x * spriteSize + 2) (y * spriteSize - 10)

drawGoblinTiles :: SDL.Renderer -> Tileset -> Int -> [Tile] -> IO ()
drawGoblinTiles renderer tileset size tiles = do
    -- TODO: blend mode should be set when we load texture
    setSpriteBlendMode goblinNeutral SDL.blendModeBlend
    setSpriteBlendMode goblin1 SDL.blendModeBlend
    setSpriteBlendMode goblin2 SDL.blendModeBlend
    setSpriteBlendMode goblin3 SDL.blendModeBlend
    setSpriteBlendMode goblin4 SDL.blendModeBlend
    mapM_ (\(i, tile) -> renderTileAtPos tile (indexToPos i size)) (zip [0..] tiles)
    where
      goblinNeutral = getSprite tileset "goblin0"
      goblin1 = getSprite tileset "goblin1"
      goblin2 = getSprite tileset "goblin2"
      goblin3 = getSprite tileset "goblin3"
      goblin4 = getSprite tileset "goblin4"
      -- TODO: this begs for refactoring
      renderTileAtPos (MineTile owner) pos =
        case owner of
          Nothing -> renderAtPos renderer goblinNeutral pos
          Just (HeroId 1) -> renderAtPos renderer goblin1 pos
          Just (HeroId 2) -> renderAtPos renderer goblin2 pos
          Just (HeroId 3) -> renderAtPos renderer goblin3 pos
          Just (HeroId 4) -> renderAtPos renderer goblin4 pos
          _ -> error $ "Unknown hero id: " ++ show owner
      renderTileAtPos _ _ = return ()

drawMineTiles :: SDL.Renderer -> Tileset -> Int -> [Tile] -> IO ()
drawMineTiles renderer tileset size tiles = do
    -- TODO: blend mode should be set when we load texture
    setSpriteBlendMode mineNeutral SDL.blendModeBlend
    setSpriteBlendMode mine1 SDL.blendModeBlend
    setSpriteBlendMode mine2 SDL.blendModeBlend
    setSpriteBlendMode mine3 SDL.blendModeBlend
    setSpriteBlendMode mine4 SDL.blendModeBlend
    mapM_ (\(i, tile) -> renderTileAtPos tile (indexToPos i size)) (zip [0..] tiles)
    where
      mineNeutral = getSprite tileset "mine0"
      mine1 = getSprite tileset "mine1"
      mine2 = getSprite tileset "mine2"
      mine3 = getSprite tileset "mine3"
      mine4 = getSprite tileset "mine4"
      -- TODO: this begs for refactoring
      renderTileAtPos (MineTile owner) pos =
        case owner of
          Nothing -> renderAtPos renderer mineNeutral pos
          Just (HeroId 1) -> renderAtPos renderer mine1 pos
          Just (HeroId 2) -> renderAtPos renderer mine2 pos
          Just (HeroId 3) -> renderAtPos renderer mine3 pos
          Just (HeroId 4) -> renderAtPos renderer mine4 pos
          _ -> error $ "Unknown hero id: " ++ show owner
      renderTileAtPos _ _ = return ()

drawHeroTiles :: SDL.Renderer -> Tileset -> [Hero] -> IO ()
drawHeroTiles renderer tileset heroes = do
    -- TODO: blend mode should be set when we load texture
    setSpriteBlendMode hero1 SDL.blendModeBlend
    setSpriteBlendMode hero2 SDL.blendModeBlend
    setSpriteBlendMode hero3 SDL.blendModeBlend
    setSpriteBlendMode hero4 SDL.blendModeBlend
    mapM_ (\hero -> renderAtPos renderer (getHeroSprite $ heroId hero) (heroPosToDraw $ heroPos hero)) heroes
    where
      hero1 = getSprite tileset "hero1"
      hero2 = getSprite tileset "hero2"
      hero3 = getSprite tileset "hero3"
      hero4 = getSprite tileset "hero4"
      heroPosToDraw Pos {posX = x, posY = y} = Pos (x * spriteSize - 4) (y * spriteSize - 8)
      -- TODO: this begs for refactoring
      getHeroSprite (HeroId x) = case x of
                                   1 -> hero1
                                   2 -> hero2
                                   3 -> hero3
                                   4 -> hero4
                                   _ -> error $ "Unknown hero id: " ++ show x

renderAtPos :: SDL.Renderer -> Sprite -> Pos -> IO ()
renderAtPos renderer sprite Pos {posX = x, posY = y} = void $ renderSprite renderer sprite x y

indexToPos :: Int -> Int -> Pos
indexToPos index size = Pos ((index `div` size) * spriteSize) ((index `mod` size) * spriteSize)

runSDLVindinium :: Settings -> SDLVindinium a -> IO a
runSDLVindinium settings (SDLVindinium v) = do
    initializeSDL [SDL.initFlagVideo] >>= catchRisky
    Image.imgInit [Image.InitPNG]
    window <- createWindow "Vindinium Viewer" >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated] >>= catchRisky
    _ <- clearWindow renderer
    tileset <- loadSprites renderer
    _ <- repeatedTimer handleNextEvent (usDelay 100) -- allows window to be responsive but doesn't handle any events
    resources <- stToIO $ initResources settings window renderer tileset
    result <- runReaderT v resources
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

handleNextEvent :: IO ()
handleNextEvent = do
    _ <- pollEvent
    return ()
