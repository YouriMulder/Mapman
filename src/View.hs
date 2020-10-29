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
viewPure gState = renderedMaze
    where
        
        renderedMaze = 
            Translate 0 ((windowBotPadding/2)-(windowTopPadding/2)) $
            Pictures [vMaze, vPacMan, vBlinky, vPinky, vInky, vClyde, vRState, vScore]
        vMaze   = renderMaze        $ maze     gState
        vPacMan = renderGridLocated $ pacman   gState
        vBlinky = renderGridLocated $ blinky   gState
        vPinky  = renderGridLocated $ pinky    gState
        vInky   = renderGridLocated $ inky     gState
        vClyde  = renderGridLocated $ clyde    gState
        vRState = renderRunState    $ runState gState
        vScore  = renderScore       $ score    gState

renderScore :: Int -> Picture
renderScore score = 
    Color white $
    Translate (-windowWidth/2) ((windowHeight/2) - (windowTopPadding*0.8)) $
    Scale 0.10 0.10 $ 
    Text ("Score  " ++ show score)

renderTextOverlay :: Float -> String -> Picture
renderTextOverlay x s = 
    Color white $
    Translate x 0 $
    Scale 0.2 0.2 $ 
    Text s

renderRunState :: RunState -> Picture
renderRunState Paused       = renderTextOverlay (-125) "Press p to continue"
renderRunState (Death n)    = renderTextOverlay (-75) $ "You Died (" ++ show (n `div` fps) ++ "...)"
renderRunState (GameOver n) = renderTextOverlay (-75) $ "Game Over (" ++ show (n `div` fps) ++ "...)"
renderRunState Normal       = Blank

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
        newPixelX = pixelX - (mazeWidth  / 2)
        newPixelY = (mazeHeight / 2) - pixelY

translateCellOrigin :: (Float, Float) -> Picture -> Picture
translateCellOrigin (width, height) = translate width (-1 * height)