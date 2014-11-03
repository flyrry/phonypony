module Vindinium.Runner.Tileset ( Sprite
                                , loadSprites
                                , renderSprite
                                , spriteSize
                                ) where

import Data.List (foldl')
import qualified Data.Map.Lazy as M
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image

import Paths_phonypony

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils (with)

data Sprite = Sprite { texture :: SDL.Texture
                     , clip :: SDL.Rect
                     }

spriteSize :: Int
spriteSize = 24

loadSprites :: SDL.Renderer -> IO (M.Map String Sprite)
loadSprites renderer = do
    lowlandsTexture <- loadTexture renderer "resources/lowlands_24.png"
    let sprites = foldl' (\m (name, x, y) -> M.insert name (Sprite lowlandsTexture (SDL.Rect (fromIntegral $ x * spriteSize) (fromIntegral $ y * spriteSize) (fromIntegral spriteSize) (fromIntegral spriteSize))) m) M.empty tilesConf
    return sprites

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
              -- water / plain
            , ("water_plain_se", 0, 0)
            , ("water_plain_s",  1, 0)
            , ("water_plain_sw", 2, 0)
            , ("water_plain_e",  0, 1)
            , ("water_plain_w",  2, 1)
            , ("water_plain_ne", 0, 2)
            , ("water_plain_n",  1, 2)
            , ("water_plain_nw", 2, 2)
            , ("plain_water_se", 3, 0)
            , ("plain_water_ne", 3, 1)
            , ("plain_water_sw", 4, 0)
            , ("plain_water_nw", 4, 1)
              -- earth / plain
            , ("earth_plain_se", 5, 0)
            , ("earth_plain_s",  6, 0)
            , ("earth_plain_sw", 7, 0)
            , ("earth_plain_e",  5, 1)
            , ("earth_plain_w",  7, 1)
            , ("earth_plain_ne", 5, 2)
            , ("earth_plain_n",  6, 2)
            , ("earth_plain_nw", 7, 2)
            , ("plain_earth_se", 8, 0)
            , ("plain_earth_ne", 8, 1)
            , ("plain_earth_sw", 9, 0)
            , ("plain_earth_nw", 9, 1)
              -- rock / plain
            , ("rock_plain_se",  5, 3)
            , ("rock_plain_s",   6, 3)
            , ("rock_plain_sw",  7, 3)
            , ("rock_plain_e",   5, 4)
            , ("rock_plain_w",   7, 4)
            , ("rock_plain_ne",  5, 5)
            , ("rock_plain_n",   6, 5)
            , ("rock_plain_nw",  7, 5)
            , ("plain_rock_se",  8, 2)
            , ("plain_rock_ne",  8, 3)
            , ("plain_rock_sw",  9, 2)
            , ("plain_rock_nw",  9, 3)
              -- empty / plain
            , ("empty_plain_se", 0, 3)
            , ("empty_plain_s",  1, 3)
            , ("empty_plain_sw", 2, 3)
            , ("empty_plain_e",  0, 4)
            , ("empty_plain_w",  2, 4)
            , ("empty_plain_ne", 0, 5)
            , ("empty_plain_n",  1, 5)
            , ("empty_plain_nw", 2, 5)
            , ("plain_empty_se", 3, 4)
            , ("plain_empty_ne", 3, 5)
            , ("plain_empty_sw", 4, 4)
            , ("plain_empty_nw", 4, 5)
              -- water / earth
            , ("water_earth_se",10, 2)
            , ("water_earth_s", 11, 2)
            , ("water_earth_sw",12, 2)
            , ("water_earth_e", 10, 3)
            , ("water_earth_w", 12, 3)
            , ("water_earth_ne",10, 4)
            , ("water_earth_n", 11, 4)
            , ("water_earth_nw",12, 4)
            , ("earth_water_se", 8, 4)
            , ("earth_water_ne", 8, 5)
            , ("earth_water_sw", 9, 4)
            , ("earth_water_nw", 9, 5)
              -- water / rock
            , ("water_rock_se", 13, 2)
            , ("water_rock_s",  14, 2)
            , ("water_rock_sw", 15, 2)
            , ("water_rock_e",  13, 3)
            , ("water_rock_w",  15, 3)
            , ("water_rock_ne", 13, 4)
            , ("water_rock_n",  14, 4)
            , ("water_rock_nw", 15, 4)
            , ("rock_water_se", 12, 0)
            , ("rock_water_ne", 12, 1)
            , ("rock_water_sw", 13, 0)
            , ("rock_water_nw", 13, 1)
              -- plain extras
            , ("plain_grass1",  10, 0)
            , ("plain_grass2",  11, 0)
            , ("plain_grass3",  11, 1)
            , ("plain_flower",  10, 1)
            ]
