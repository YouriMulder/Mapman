module View where

import Model

import Graphics.Gloss
import Data.List
import Pacman
import Blinky

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gameState = Pictures [vPacMan, vBlinky, vPinky, vInky, vClyde]
    where
        vPacMan = renderInGrid $ pacman gameState
        vBlinky = renderInGrid $ blinky gameState
        vPinky  = renderInGrid $ pinky  gameState
        vInky   = renderInGrid $ inky   gameState
        vClyde  = renderInGrid $ clyde  gameState

renderInGrid :: (Sprite a) => a -> Picture
renderInGrid a = translateInGrid (render a) a

translateInGrid :: (GridLocated a) => Picture -> a -> Picture
translateInGrid picture a = translateCellOrigin (cellWidth/2, cellHeight/2) (translateTopLeft pixelPosition picture)
    where 
        pixelPosition = getCellPixelTopLeft (getLocation a)

getCellPixelTopLeft :: Model.Point -> (Float, Float)
getCellPixelTopLeft (Point cellX cellY) = 
    (fromIntegral cellX * cellWidth, fromIntegral cellY * cellHeight) 

translateTopLeft :: (Float, Float) -> Picture -> Picture
translateTopLeft (pixelX, pixelY) =
    translate newPixelX newPixelY
    where 
        newPixelX = pixelX - (fromIntegral windowWidth  / 2)
        newPixelY = (fromIntegral windowHeight / 2) - pixelY

translateCellOrigin :: (Float, Float) -> Picture -> Picture
translateCellOrigin (width, height) = translate width (-1 * height)