module ControllerPacMan where

import Model
import State
import Maze

instance GridLocated PacMan where
    move = undefined
    getLocation (PacMan p _)   = p
    setLocation (PacMan _ pDirection) position = PacMan position pDirection
    

updatePacMan :: GameState -> GameState
updatePacMan gstate = setGameStatePacMan (movePacMan (pacman gstate) gstate) gstate

movePacMan :: PacMan -> GameState -> PacMan
movePacMan pacMan@(PacMan pPosition pDirection) GameState{maze=m} = 
    case isValidDirection pDirection pPosition m of
        True -> PacMan (moveFrom pPosition pDirection) pDirection
        False -> pacMan

updatePacManDirection :: Direction -> PacMan -> Maze -> PacMan
updatePacManDirection direction pacMan@(PacMan pPosition _) m =
    case isValidDirection direction pPosition m of
        True -> setPacManDirection direction pacMan
        _    -> pacMan

setPacManDirection :: Direction -> PacMan -> PacMan
setPacManDirection direction (PacMan pPosition _) = PacMan pPosition direction

pacManDeath :: GameState -> GameState
pacManDeath gs@GameState{lives=0} = gameOver gs
pacManDeath gs@GameState{lives=l} = (resetState gs){lives=l - 1}

interact :: PacMan -> Ghost -> Maybe Ghost
-- return Nothing if we died, return a new ghost if we didn't
-- if our position is not the same, and we were also not in the spot the ghost came from:
interact PacMan{ppos=pp} g@Ghost{gpos=gp, gdir=gd} 
       | pp /= gp && pp /= moveFrom gp (opposite gd) = Just g               -- no interaction
interact _ g@Ghost{gstate=(Scared _)}                = Just g{gstate=Dead}  -- kill the ghost
interact _ g@Ghost{gstate=Dead}                      = Just g               -- no interaction
interact _  _                                        = Nothing              -- kill pacman

interactState :: GameState -> Maybe GameState
interactState gs@GameState{
    blinky=gb,
    pinky=gp,
    inky=gi,
    clyde=gc
} = do
        interactBlinky <- (interactSingle gb (\g _gs -> _gs{blinky=g})) gs
        interactPinky  <- (interactSingle gp (\g _gs -> _gs{pinky=g})) interactBlinky
        interactInky   <- (interactSingle gi (\g _gs -> _gs{inky=g})) interactPinky
        interactClyde  <- (interactSingle gc (\g _gs -> _gs{clyde=g})) interactInky

        return interactClyde
    where 

        interactSingle :: Ghost -> (Ghost -> GameState -> GameState) -> GameState -> Maybe GameState
        -- update single ghost interaction based on ghost setter function
        -- once the state has been reset, the other ghosts can't kill pacman anymore
        -- this is because the state will be reset, and pacman does not overlap with any of the ghosts
        interactSingle g setGhost gs@GameState {pacman=pm, score=s} = case ControllerPacMan.interact pm g of
            Nothing -> Nothing
            Just g  -> (Just (setGhost g gs){score=s + ghostKillScore})