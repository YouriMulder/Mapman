module Controller where

import Model
import Ghosts
import ControllerPacMan

import qualified Data.Set as S
import System.Random

{- debugging stuff -}
import Maze
{- /debugging stuff -}

import ControllerPacMan

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gstate@GameState{lives=l} = do
    print l

    gen <- newStdGen
    gstateGhosts <- return $ updateGhosts gstate $ randomPos gen
    gstatePacMan <- return $ updatePacMan gstateGhosts
    gstateInput  <- return $ handleKeysPressed gstatePacMan
    return                 $ ControllerPacMan.interactState gstateInput

    where -- only needed for scared/player controlled ghosts, others are already handled in the updateGhosts function:
          randPoint :: StdGen -> Model.Point
          randPoint seed = let (x, ySeed) = randomR (0, mazeWidth) seed in (Point x $ fst $ randomR (0, mazeHeight) ySeed) 

          randomPos :: StdGen -> Ghost -> Maybe Model.Point
          randomPos seed Ghost{gstate=(Scared _)}                 = Just $ randPoint seed

          -- todo: bij keypresses de direction van de bestuurde ghost aanpassen

handleKeysPressed :: GameState -> GameState
handleKeysPressed gstate 
    = foldr keyHandler gstate (S.toList (keysPressed gstate))

keyHandler :: Key -> GameState -> GameState
keyHandler (Char c) gstate
    = charKeyHandler c gstate
keyHandler (SpecialKey k) gstate 
    = specialKeyHandler k gstate
keyHandler _ gstate = gstate

charKeyHandler :: Char -> GameState -> GameState
charKeyHandler _ gstate = gstate

specialKeyHandler :: SpecialKey -> GameState -> GameState
specialKeyHandler KeyUp    gstate = updatePacManDirection' North gstate
specialKeyHandler KeyDown  gstate = updatePacManDirection' South gstate
specialKeyHandler KeyLeft  gstate = updatePacManDirection' West  gstate
specialKeyHandler KeyRight gstate = updatePacManDirection' East  gstate
specialKeyHandler _        gstate = gstate 

updatePacManDirection' d gstate = 
    setGameStatePacMan (updatePacManDirection d (pacman gstate) (maze gstate)) gstate

-- step :: Float -> GameState -> IO GameState
-- step secs = return


input :: Event -> GameState -> IO GameState
input keyEvent@EventKey{} gstate
    = return (updateKeysInput keyEvent gstate)
input _ gstate = return gstate

updateKeysInput :: Event -> GameState -> GameState
updateKeysInput e@(EventKey k Down   _ _) gstate 
    = gstate { keysPressed = S.insert k (keysPressed gstate) }
updateKeysInput e@(EventKey k Up _ _) gstate 
    = gstate { keysPressed = S.delete k (keysPressed gstate) }