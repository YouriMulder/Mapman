module View where

import Model

import Graphics.Gloss
import Data.List

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gameState = vPacMan
    where
        vPacMan = viewPacMan $ pacman gameState

viewPacMan :: PacMan -> Picture
viewPacMan pacMan = 
    translateInGrid pacManCircleInMiddle (getPacManCellPos pacMan)
    where
        pacManCircleInMiddle = translateCellOrigin (cellWidth, cellWidth) pacManCircle
        pacManCircle = Color yellow $ Circle cellWidth

cellWidth :: Float
cellWidth = fromIntegral windowWidth / fromIntegral mazeWidth

cellHeight :: Float
cellHeight = fromIntegral windowHeight / fromIntegral mazeHeight

cellToPixel :: Model.Point -> (Float, Float)
cellToPixel (Point cellX cellY) = 
    (fromIntegral cellX * cellWidth, fromIntegral cellY * cellHeight) 

translateInGrid :: Picture -> Model.Point -> Picture
translateInGrid picture cell = 
    translateTopLeft pixelPosition picture
    where 
        pixelPosition = cellToPixel cell 

translateTopLeft :: (Float, Float) -> Picture -> Picture
translateTopLeft (pixelX, pixelY) =
    translate newPixelX newPixelY
    where 
        newPixelX = pixelX - (fromIntegral windowWidth  / 2)
        newPixelY = pixelY + (fromIntegral windowHeight / 2)

translateCellOrigin :: (Float, Float) -> Picture -> Picture
translateCellOrigin (width, height) = translate width (-1 * height)