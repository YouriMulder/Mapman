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
            Pictures [vMaze, vPacMan, vBlinky, vPinky, vInky, vClyde, vPaused, vScore, vHighScore, vLives]
        vMaze   = renderMaze        $ maze   gState
        vPacMan = renderGridLocated $ pacman gState
        vBlinky = renderGridLocated $ blinky gState
        vPinky  = renderGridLocated $ pinky  gState
        vInky   = renderGridLocated $ inky   gState
        vClyde  = renderGridLocated $ clyde  gState
        vPaused = renderPaused      $ paused gState
        vScore  = renderScore       $ score  gState
        vHighScore = renderHighScore $ highScore gState 
        vLives  = renderLives       $ lives gState

renderLives :: Int -> Picture
renderLives lives = 
    Color white $
    Translate (-windowWidth/2) ((-windowHeight/2) + (windowBotPadding*0.2)) $
    Scale 0.10 0.10 $ 
    Text ("lives  " ++ show lives)

renderHighScore :: Int -> Picture
renderHighScore highScore = 
    Color white $
    Translate (0) ((windowHeight/2) - (windowTopPadding*0.8)) $
    Scale 0.10 0.10 $ 
    Text ("HighScore  " ++ show highScore)

renderScore :: Int -> Picture
renderScore score = 
    Color white $
    Translate (-windowWidth/2) ((windowHeight/2) - (windowTopPadding*0.8)) $
    Scale 0.10 0.10 $ 
    Text ("Score  " ++ show score)

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
        newPixelX = pixelX - (mazeWidth  / 2)
        newPixelY = (mazeHeight / 2) - pixelY

translateCellOrigin :: (Float, Float) -> Picture -> Picture
translateCellOrigin (width, height) = translate width (-1 * height)