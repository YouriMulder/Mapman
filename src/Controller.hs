module Controller where

import Model
import Ghosts

import System.Random

{- debugging stuff -}
import Maze
{- /debugging stuff -}

import ControllerPacMan

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step secs gstate = do
    gen <- newStdGen
    gstateGhosts <- return $ updateGhosts gstate $ randomPos gen
    gstatePacMan <- return $ updatePacMan gstateGhosts
    return gstatePacMan

    where -- only needed for scared/player controlled ghosts, others are already handled in the updateGhosts function:
          randPoint :: StdGen -> Model.Point
          randPoint seed = let (x, ySeed) = randomR (0, mazeWidth) seed in (Point x $ fst $ randomR (0, mazeHeight) ySeed) 

          randomPos :: StdGen -> Ghost -> Maybe Model.Point
          randomPos seed Ghost{gstate=(Scared _)}                 = Just $ randPoint seed

          -- todo: bij keypresses de direction van de bestuurde ghost aanpassen

-- step :: Float -> GameState -> IO GameState
-- step secs = return


input :: Event -> GameState -> IO GameState
input keyEvent@EventKey{} gstate
    = return (eventKeyHandler keyEvent gstate)
input _ gstate = return gstate

eventKeyHandler :: Event -> GameState -> GameState
eventKeyHandler (EventKey (Char c) _ _ _) gstate
    = keyHandler c gstate
eventKeyHandler (EventKey (SpecialKey k) _ _ _) gstate 
    = specialKeyHandler k gstate
eventKeyHandler _ gstate = gstate

keyHandler :: Char -> GameState -> GameState
keyHandler _ gstate = gstate

specialKeyHandler :: SpecialKey -> GameState -> GameState
specialKeyHandler KeyUp    gstate = updatePacManDirection' North gstate
specialKeyHandler KeyDown  gstate = updatePacManDirection' South gstate
specialKeyHandler KeyLeft  gstate = updatePacManDirection' West  gstate
specialKeyHandler KeyRight gstate = updatePacManDirection' East  gstate
specialKeyHandler _        gstate = gstate 

updatePacManDirection' d gstate = 
    setGameStatePacMan (updatePacManDirection d (pacman gstate) (maze gstate)) gstate
