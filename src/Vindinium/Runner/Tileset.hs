module Vindinium.Runner.Tileset ( Sprite
                                , Tileset
                                , spriteSize
                                , biggerSpriteSize
                                , loadSprites
                                , renderSprite
                                , getSprite
                                , setSpriteBlendMode
                                ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import qualified Data.Map.Lazy as M
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image

import Paths_phonypony

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils (with)

-- TODO: make clip Maybe
data Sprite = Sprite { texture :: SDL.Texture
                     , clip :: SDL.Rect
                     } deriving Show

type Tileset = M.Map String Sprite

spriteSize :: Int
spriteSize = 24

biggerSpriteSize :: Int
biggerSpriteSize = 32

getSprite :: Tileset -> String -> Sprite
getSprite tileset name =
    fromMaybe
      (error $ "Trying to load a sprite which doesn't exist: " ++ name)
      (M.lookup name tileset)

setSpriteBlendMode :: Sprite -> SDL.BlendMode -> IO ()
setSpriteBlendMode sprite blendMode = void $ SDL.setTextureBlendMode (texture sprite) blendMode

loadSprites :: SDL.Renderer -> IO Tileset
loadSprites renderer = do
    -- TODO: this begs for refactoring
    lowlandsTexture <- loadTexture renderer "resources/lowlands_24.png"
    treeTexture <- loadTexture renderer "resources/tree.png"
    tavernTexture <- loadTexture renderer "resources/beer.png"
    heroesTexture <- loadTexture renderer "resources/heroes.png"
    goblinsTexture <- loadTexture renderer "resources/goblins.png"
    minesTexture <- loadTexture renderer "resources/mines.png"
    let spriteSizeCInt = fromIntegral spriteSize :: CInt
        biggerSpriteSizeCInt = fromIntegral biggerSpriteSize :: CInt
        resurrectionMarkSprites = foldl' (\m (name, x, y) -> M.insert name (Sprite heroesTexture (SDL.Rect (fromIntegral $ x * biggerSpriteSize) (fromIntegral $ y * biggerSpriteSize) biggerSpriteSizeCInt biggerSpriteSizeCInt)) m) M.empty rezMarks
        goblinsSprites = foldl' (\m (name, x, y) -> M.insert name (Sprite goblinsTexture (SDL.Rect (fromIntegral $ x * biggerSpriteSize) (fromIntegral $ y * biggerSpriteSize) biggerSpriteSizeCInt biggerSpriteSizeCInt)) m) M.empty goblinsConf
        mineSprites = foldl' (\m (name, x, y) -> M.insert name (Sprite minesTexture (SDL.Rect (fromIntegral $ x * biggerSpriteSize) (fromIntegral $ y * biggerSpriteSize) biggerSpriteSizeCInt biggerSpriteSizeCInt)) m) M.empty minesConf
        heroesSprites = foldl' (\m (name, x, y) -> M.insert name (Sprite heroesTexture (SDL.Rect (fromIntegral $ x * biggerSpriteSize) (fromIntegral $ y * biggerSpriteSize) biggerSpriteSizeCInt biggerSpriteSizeCInt)) m) M.empty heroesConf
        treeSprite = Sprite treeTexture (SDL.Rect 0 0 spriteSizeCInt spriteSizeCInt)
        tavernSprite = Sprite tavernTexture (SDL.Rect 0 0 spriteSizeCInt biggerSpriteSizeCInt)
        lowlandsSprites = foldl' (\m (name, x, y) -> M.insert name (Sprite lowlandsTexture (SDL.Rect (fromIntegral $ x * spriteSize) (fromIntegral $ y * spriteSize) spriteSizeCInt spriteSizeCInt)) m) M.empty tilesConf
        sprites = M.insert "tavern" tavernSprite (M.insert "tree" treeSprite lowlandsSprites)
    return (M.union heroesSprites $ M.union mineSprites $ M.union goblinsSprites $ M.union resurrectionMarkSprites sprites)

renderSprite :: SDL.Renderer -> Sprite -> Int -> Int -> IO CInt
renderSprite renderer sprite x y =
    with (clip sprite) $ \clipPtr ->
      with (SDL.Rect (fromIntegral x) (fromIntegral y) (SDL.rectW $ clip sprite) (SDL.rectH $ clip sprite)) $ \renderQuadPtr ->
        SDL.renderCopy renderer (texture sprite) clipPtr renderQuadPtr

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path = do
    filePath <- getDataFileName path
    Image.imgLoadTexture renderer filePath >>= catchRisky

type Risky a = Either String a

catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return

throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)

tilesConf :: [(String, Int, Int)]
tilesConf = [ -- primitive tiles
              ("plain",          1, 1)
            , ("water",          3, 2)
            , ("earth",          4, 2)
            , ("rock",           4, 3)
            , ("empty",          3, 3)
              -- plain extras
            , ("plain_grass1",  10, 0)
            , ("plain_grass2",  11, 0)
            , ("plain_grass3",  11, 1)
            , ("plain_flower",  10, 1)
            ]

rezMarks :: [(String, Int, Int)]
rezMarks = [ ("rez1", 4, 3)
           , ("rez2", 4, 2)
           , ("rez3", 4, 1)
           , ("rez4", 4, 0)
           ]

heroesConf :: [(String, Int, Int)]
heroesConf = [ ("hero1", 0, 3)
             , ("hero2", 0, 2)
             , ("hero3", 0, 1)
             , ("hero4", 0, 0)
             ]

goblinsConf :: [(String, Int, Int)]
goblinsConf = [ ("goblin0", 0, 0)
              , ("goblin1", 0, 1)
              , ("goblin2", 0, 2)
              , ("goblin3", 0, 3)
              , ("goblin4", 0, 4)
              ]

minesConf :: [(String, Int, Int)]
minesConf = [ ("mine0", 0, 0)
            , ("mine1", 0, 3)
            , ("mine2", 0, 4)
            , ("mine3", 0, 2)
            , ("mine4", 0, 1)
            ]
