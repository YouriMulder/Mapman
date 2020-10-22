module ControllerPacMan where

import Model
import Maze

instance GridLocated PacMan where
    move = undefined
    getLocation (PacMan p _ _)   = p
    setLocation (PacMan _ pDirection pState) position = PacMan position pDirection pState
    

updatePacMan :: GameState -> GameState
updatePacMan gstate = setGameStatePacMan (movePacMan (pacman gstate) gstate) gstate

movePacMan :: PacMan -> GameState -> PacMan
movePacMan pacMan@(PacMan pPosition pDirection pState) GameState{maze=m} = 
    case isValidDirection pDirection pPosition m of
        True -> PacMan (moveFrom pPosition pDirection) pDirection pState
        False -> pacMan

updatePacManDirection :: Direction -> PacMan -> Maze -> PacMan
updatePacManDirection direction pacMan@(PacMan pPosition _ _) m =
    case isValidDirection direction pPosition m of
        True -> setPacManDirection direction pacMan
        _    -> pacMan

setPacManDirection :: Direction -> PacMan -> PacMan
setPacManDirection direction (PacMan pPosition _ pState) = PacMan pPosition direction pState


