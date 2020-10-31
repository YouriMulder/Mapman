module Controller where

import Model
    ( GameState(GameState, score, blinky, inky, pinky, clyde, pacman,
                maze, keysPressed, runState),
      RunState(Paused, Death, GameOver, Victory, Normal),
      Controllable(setDirection),
      mapGhosts )
import ModelBase ( Direction(..), Point(..) )
import ModelGhost ( Ghost(Ghost, gstate), GhostState(Scared) )
import ModelMaze
    ( mazeAmountOfCellsWidth, mazeAmountOfCellsHeight )
import ControllerGhost
import ControllerPacMan
    ( updatePacMan,
      updatePacManDirection,
      pacManDeath,
      interactState,
      interactMaze )
import Serial ( checkDumpState, checkLoadState )
import State ( updateScore, victoryCheck, gameOver )

import qualified Data.Set as S
import System.Random ( StdGen, Random(randomR), newStdGen )
import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey, Char),
      KeyState(Up, Down),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft),
      Event(EventKey) )

-- | Function to handle every update cycle. 
-- | Updates every single entity in the GameState for a cycle.
step :: Float -> GameState -> IO GameState
step _    gstate@GameState{runState=Paused}              = return $ handleKeysPressed gstate
step _    gstate@GameState{runState=Death 0}             = return $ gstate{runState=Normal}
step _    gstate@GameState{runState=Death n}             = return $ gstate{runState=Death $ n - 1}
step _    gstate@GameState{runState=GameOver 0}          = return $ gstate{runState=Normal}
step _    gstate@GameState{runState=GameOver n}          = return $ gstate{runState=GameOver $ n - 1}
step _    gstate@GameState{runState=Victory 0, score=s}  = return $ (gameOver gstate){runState=Normal, score=s}
step _    gstate@GameState{runState=Victory n}           = return $ gstate{runState=Victory $ n - 1}
step secs gstate = do
    gen <- newStdGen

    checkDumpState gstate
    loadedState <- checkLoadState gstate
    
    let finalState = 
            case ControllerPacMan.interactState
                . ControllerPacMan.interactMaze
                . updatePacMan
                . updateGhosts (randomPos gen)
                . handleKeysPressed $ loadedState
            of 
                Nothing -> pacManDeath gstate
                Just gs -> gs

    -- check if victory has been achieved!
    return $ victoryCheck 
           . updateScore $ finalState

    where -- only needed for scared/player controlled ghosts, others are already handled in the updateGhosts function:
          randPoint :: StdGen -> ModelBase.Point
          randPoint seed = let (x, ySeed) = randomR (0, mazeAmountOfCellsWidth) seed in (Point x $ fst $ randomR (0, mazeAmountOfCellsHeight) ySeed) 

          randomPos :: StdGen -> Ghost -> Maybe ModelBase.Point
          randomPos seed Ghost{gstate=(Scared _)}                 = Just $ randPoint seed

-- | Handles all the current pressed keys.
handleKeysPressed :: GameState -> GameState
handleKeysPressed gstate 
    = foldr keyHandler gstate (S.toList (keysPressed gstate))

-- | Handles a single key, and updates the GameState. 
keyHandler :: Key -> GameState -> GameState
keyHandler (Char c) gstate
    = charKeyHandler c gstate
keyHandler (SpecialKey k) gstate 
    = specialKeyHandler k gstate
keyHandler _ gstate = gstate

-- | Handles a single character key, and updates the GameState.
charKeyHandler :: Char -> GameState -> GameState
charKeyHandler 'p'  gstate = togglePause gstate
charKeyHandler 'w'  gstate = mapGhosts gstate (flip setDirection North)
charKeyHandler 'a'  gstate = mapGhosts gstate (flip setDirection West)
charKeyHandler 's'  gstate = mapGhosts gstate (flip setDirection South)
charKeyHandler 'd'  gstate = mapGhosts gstate (flip setDirection East)
charKeyHandler '0'  gstate = setGhostsComputerControlled gstate
charKeyHandler '9'  gstate =
    (setGameStateGhostPlayer (blinky gstate) . setGhostsComputerControlled) gstate
charKeyHandler '8'  gstate =
    (setGameStateGhostPlayer (inky gstate)   . setGhostsComputerControlled) gstate
charKeyHandler '7'  gstate =
    (setGameStateGhostPlayer (pinky gstate)  . setGhostsComputerControlled) gstate
charKeyHandler '6'  gstate =
    (setGameStateGhostPlayer (clyde gstate)  . setGhostsComputerControlled) gstate
charKeyHandler _    gstate = gstate

-- | Handles a special key, and updates the GameState.
specialKeyHandler :: SpecialKey -> GameState -> GameState
specialKeyHandler KeyUp    gstate = updatePacManDirection' North gstate
specialKeyHandler KeyDown  gstate = updatePacManDirection' South gstate
specialKeyHandler KeyLeft  gstate = updatePacManDirection' West  gstate
specialKeyHandler KeyRight gstate = updatePacManDirection' East  gstate
specialKeyHandler _        gstate = gstate 

-- | Updates the direction where PacMan is going.
updatePacManDirection' :: Direction -> GameState -> GameState
updatePacManDirection' d gstate = 
    gstate{pacman = updatePacManDirection d (pacman gstate) (maze gstate)}

-- | Handles all the received events. 
input :: Event -> GameState -> IO GameState
input keyEvent@EventKey{} gstate
    = return (updateKeysInput keyEvent gstate)
input _ gstate = return gstate

-- | Adds a key to the keysPressed when Down, and deletes the key if Up.
updateKeysInput :: Event -> GameState -> GameState
updateKeysInput (EventKey k Down   _ _) gstate 
    = gstate { keysPressed = S.insert k (keysPressed gstate) }
updateKeysInput (EventKey k Up _ _) gstate 
    = gstate { keysPressed = S.delete k (keysPressed gstate) }

-- | Toggle the pause state. 
togglePause :: GameState -> GameState
togglePause gstate@GameState{runState=Paused} = gstate {runState=Normal}
togglePause gstate@GameState{runState=Normal} = gstate {runState=Paused}
togglePause gstate                            = gstate