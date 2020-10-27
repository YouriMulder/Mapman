module View where

import Model
import ViewPacMan
import ViewGhost
import ViewMaze
import Maze

import Data.List
import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gState = Pictures [vMaze, vPacMan, vBlinky, vPinky, vInky, vClyde, vPaused]
    where
        vMaze   = renderMaze        $ maze   gState
        vPacMan = renderGridLocated $ pacman gState
        vBlinky = renderGridLocated $ blinky gState
        vPinky  = renderGridLocated $ pinky  gState
        vInky   = renderGridLocated $ inky   gState
        vClyde  = renderGridLocated $ clyde  gState
        vPaused = renderPaused      $ paused gState

renderPaused :: Pause -> Picture
renderPaused IsPaused = 
    Color white $
    Translate (-125) 0 $
    Scale 0.2 0.2 $ 
    Text "Press p to continue"
renderPaused _        = Blank 


renderMaze :: Maze -> Picture
renderMaze maze = Pictures $ renderedFields
    where
        renderedFields = map (uncurry (flip renderInGrid)) (getAllPairs maze)

renderGridLocated :: (Sprite a, GridLocated a) => a -> Picture
renderGridLocated a = translateInGrid (render a) (getLocation a)

renderInGrid :: (Sprite a) => a -> Model.Point -> Picture
renderInGrid sprite gridPosition = translateInGrid (render sprite) gridPosition

translateInGrid :: Picture -> Model.Point -> Picture
translateInGrid picture gridPosition = 
    translateCellOrigin cellOriginOffset (translateTopLeft pixelPosition picture)
    where 
        cellOriginOffset = (cellWidth/2, cellHeight/2)
        pixelPosition = getCellPixelTopLeft gridPosition

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