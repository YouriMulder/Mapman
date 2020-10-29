module Controller where

import Model
import Ghosts
import ControllerPacMan
import ControllerGhosts
import Serial

import qualified Data.Set as S
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step _    gstate@GameState{paused=IsPaused} = 
    return $ handleKeysPressed gstate
step secs gstate = do
    gen <- newStdGen

    checkDumpState gstate
    loadedState <- checkLoadState gstate

    gstateGhosts <- return $ updateGhosts loadedState $ randomPos gen
    gstatePacMan <- return $ updatePacMan gstateGhosts
    gstateInput  <- return $ handleKeysPressed gstatePacMan
    return                 $ case ControllerPacMan.interactState gstateInput of 
                                Nothing -> pacManDeath gstateInput
                                Just gs -> gs

    where -- only needed for scared/player controlled ghosts, others are already handled in the updateGhosts function:
          randPoint :: StdGen -> Model.Point
          randPoint seed = let (x, ySeed) = randomR (0, mazeWidth) seed in (Point x $ fst $ randomR (0, mazeHeight) ySeed) 

          randomPos :: StdGen -> Ghost -> Maybe Model.Point
          randomPos seed Ghost{gstate=(Scared _)}                 = Just $ randPoint seed

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
charKeyHandler 'p'  gstate = togglePause gstate
charKeyHandler 'w'  gstate = foldGhosts gstate (flip setDirection North)
charKeyHandler 'a'  gstate = foldGhosts gstate (flip setDirection West)
charKeyHandler 's'  gstate = foldGhosts gstate (flip setDirection South)
charKeyHandler 'd'  gstate = foldGhosts gstate (flip setDirection East)
charKeyHandler _    gstate = gstate

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

togglePause :: GameState -> GameState
togglePause gstate@GameState{paused=IsPaused} = gstate {paused=NotPaused}
togglePause gstate                            = gstate {paused=IsPaused}
