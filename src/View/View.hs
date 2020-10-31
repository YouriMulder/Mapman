module View where

import Model
import ModelMaze
import ModelBase
import ModelWindow
import ModelGhost
import ControllerPacMan()
import ControllerGhost()
import ViewPacMan()
import ViewGhost()
import ViewMaze()
import Maze

import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gState = renderedMaze
    where
        
        renderedMaze = 
            Translate 0 ((windowBotPadding/2)-(windowTopPadding/2)) $
            Pictures [vMaze, vBlinky, vPinky, vInky, vClyde, vPacMan, vRState, vScore, vHighScore, vLives, vPlayer2]
        vMaze      = renderMaze        $ maze      gState
        vPacMan    = renderGridLocated $ pacman    gState
        vBlinky    = renderGridLocated $ blinky    gState
        vPinky     = renderGridLocated $ pinky     gState
        vInky      = renderGridLocated $ inky      gState
        vClyde     = renderGridLocated $ clyde     gState
        vRState    = renderRunState    $ runState  gState
        vScore     = renderScore       $ score     gState
        vHighScore = renderHighScore   $ highScore gState 
        vLives     = renderLives       $ lives     gState
        vPlayer2   = renderPlayer2                 gState

renderPlayer2 :: GameState -> Picture
renderPlayer2 gstate = case playerControlledGhosts gstate of
    []     -> Blank
    (x:_)  -> Color white $
                Translate 0 ((-windowHeight/2) + (windowBotPadding*0.2)) $
                Scale 0.10 0.10 $ 
                Text ("Player 2 " ++ show (gname x))

renderTextOverlay :: (Float, Float) -> Float -> String -> Picture
renderTextOverlay (x, y) s text = 
    Color white $
    Translate x y $
    Scale s s $ 
    Text text

renderLives :: Int -> Picture
renderLives lives = 
    renderTextOverlay (x, y) 0.1 $ "lives " ++ show lives
    where x = -windowWidth/2
          y = (-windowHeight/2) + (windowBotPadding*0.2)

renderHighScore :: Int -> Picture
renderHighScore highScore = 
    renderTextOverlay (x, y) 0.1 $ "HighScore " ++ show highScore
    where x = 0
          y = (windowHeight/2) - (windowTopPadding*0.8)

renderScore :: Int -> Picture
renderScore score = 
    renderTextOverlay (x, y) 0.1 $ "Score " ++ show score
    where x = -windowWidth/2
          y = (windowHeight/2) - (windowTopPadding*0.8)

renderRunState :: RunState -> Picture
renderRunState Paused       = renderTextOverlay (-125, 0) 0.2   "Press p to continue"
renderRunState (Death n)    = renderTextOverlay (-75, 0)  0.2 $ "You Died ("  ++ show ((n + fps - 1) `div` fps) ++ "...)"
renderRunState (GameOver n) = renderTextOverlay (-75, 0)  0.2 $ "Game Over (" ++ show ((n + fps - 1) `div` fps) ++ "...)"
renderRunState (Victory n)  = renderTextOverlay (-75, 0)  0.2 $ "You Won! ("  ++ show ((n + fps - 1) `div` fps) ++ "...)"
renderRunState Normal       = Blank

renderMaze :: Maze -> Picture
renderMaze maze = Pictures renderedFields
    where
        renderedFields = map (uncurry (flip renderInGrid)) (getAllPairs maze)

renderGridLocated :: (Sprite a, GridLocated a) => a -> Picture
renderGridLocated a = translateInGrid (render a) (getLocation a)

renderInGrid :: (Sprite a) => a -> ModelBase.Point -> Picture
renderInGrid sprite = translateInGrid (render sprite)

translateInGrid :: Picture -> ModelBase.Point -> Picture
translateInGrid picture gridPosition = 
    translateCellOrigin cellOriginOffset (translateTopLeft pixelPosition picture)
    where 
        cellOriginOffset = (cellWidth/2, cellHeight/2)
        pixelPosition = getCellPixelTopLeft gridPosition

getCellPixelTopLeft :: ModelBase.Point -> (Float, Float)
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