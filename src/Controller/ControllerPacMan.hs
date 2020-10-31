module ControllerPacMan where

import Model
    ( GameState(GameState, lives, runState, blinky, pinky, inky, clyde,
                pacman, score, maze),
      RunState(Death),
      Controllable(..),
      GridLocated(getLocation, setLocation),
      dotScore,
      palletScore,
      fps,
      mapGhosts,
      ghostKillScore )
import ModelBase ( Direction, opposite )
import ModelPacMan ( PacMan(..) )
import ModelGhost
    ( Ghost(Ghost, gpos, gdir, gstate), GhostState(Dead, Scared) )
import ModelMaze ( Maze, Field(Pellet, Dot), moveFrom )
import State ( resetState, gameOver )
import Maze ( getField, deleteField, isValidDirection )
import ControllerGhost ( scare )

instance Controllable PacMan where
    setDirection pm d = pm{pdir=d}

instance GridLocated PacMan where
    getLocation (PacMan p _)   = p
    setLocation (PacMan _ pDirection) position = PacMan position pDirection

updatePacMan :: GameState -> GameState
updatePacMan gstate = gstate{pacman = movePacMan (pacman gstate) gstate}

movePacMan :: PacMan -> GameState -> PacMan
movePacMan pacMan@(PacMan pPosition pDirection) GameState{maze=m} 
        | isValidDirection pDirection pPosition m = pacMan{ppos=moveFrom pPosition pDirection}
movePacMan pacMan                               _ = pacMan            

updatePacManDirection :: Direction -> PacMan -> Maze -> PacMan
updatePacManDirection direction pacMan@(PacMan pPosition _) m 
        | isValidDirection direction pPosition m              = setDirection pacMan direction
updatePacManDirection _         pacMan                      _ = pacMan

-- Dies at 1 because of decrement in this function
pacManDeath :: GameState -> GameState
pacManDeath gs@GameState{lives=1} = gameOver gs
pacManDeath gs@GameState{lives=l} = (resetState gs){lives=l - 1, runState=Death $ 3 * fps}

data Interaction = NoInteraction
                 | GhostKilled
                 | PacmanKilled

interact :: PacMan -> Ghost -> (Ghost, Interaction)
-- return Nothing if we died, return a new ghost if we didn't
-- if our position is not the same, and we were also not in the spot the ghost came from:
interact PacMan{ppos=pp} g@Ghost{gpos=gp, gdir=gd} 
       | pp /= gp && pp /= moveFrom gp (opposite gd) = (g, NoInteraction)
interact _ g@Ghost{gstate=(Scared _)}                = (g{gstate=Dead}, GhostKilled)
interact _ g@Ghost{gstate=Dead}                      = (g, NoInteraction)
interact _ g                                         = (g, PacmanKilled)

interactState :: GameState -> Maybe GameState
interactState gs@GameState{
    blinky=gb,
    pinky=gp,
    inky=gi,
    clyde=gc
} = do
        interactBlinky <- interactSingle gb (\g _gs -> _gs{blinky=g}) gs
        interactPinky  <- interactSingle gp (\g _gs -> _gs{pinky=g})  interactBlinky
        interactInky   <- interactSingle gi (\g _gs -> _gs{inky=g})   interactPinky
        interactSingle                   gc (\g _gs -> _gs{clyde=g})  interactInky
    where 

        interactSingle :: Ghost -> (Ghost -> GameState -> GameState) -> GameState -> Maybe GameState
        -- update single ghost interaction based on ghost setter function
        -- once the state has been reset, the other ghosts can't kill pacman anymore
        -- this is because the state will be reset, and pacman does not overlap with any of the ghosts
        interactSingle g setGhost gs@GameState {pacman=pm, score=s} = case ControllerPacMan.interact pm g of
            (_    , PacmanKilled)  -> Nothing
            (ghost, GhostKilled)   -> Just (setGhost ghost gs){score=s + ghostKillScore}
            (ghost, NoInteraction) -> Just (setGhost ghost gs)
            
interactMaze :: GameState -> GameState
interactMaze gstate@GameState{pacman=pm, score=s, maze=m} = 
    case Maze.getField pmPosition m of
        Dot     -> gstate{score=s + dotScore, maze=deleteField pmPosition m}
        Pellet  -> mapGhosts gstate{score=s + palletScore, maze=deleteField pmPosition m} scare
        _       -> gstate
    where pmPosition = getLocation pm
