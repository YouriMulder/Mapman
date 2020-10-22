module Pacman(
    interactState
) where

import Model
import Ghosts
import State
import Graphics.Gloss

pacmanDeath :: GameState -> GameState
pacmanDeath gs@GameState{lives=0} = gameOver gs
pacmanDeath gs@GameState{lives=l} = (resetState gs){lives=l - 1}

interact :: PacMan -> Ghost -> Maybe Ghost
-- return Nothing if we died, return a new pacman and ghost pair if we didn't
interact PacMan{ppos=pp} g@Ghost{gpos=gp} | pp /= gp = Just g               -- no interaction
interact _ g@Ghost{gstate=(Scared _)}                = Just g{gstate=Dead}  -- kill the ghost
interact _ g@Ghost{gstate=Dead}                      = Just g               -- no interaction
interact _  _                                        = Nothing              -- kill pacman

interactState :: GameState -> GameState
interactState gs@GameState{
    blinky=gb,
    pinky=gp,
    inky=gi,
    clyde=gc
} = interactAll gs
    where 

        interactSingle :: Ghost -> (Ghost -> GameState -> GameState) -> GameState -> GameState
        -- update single ghost interaction based on ghost setter function
        -- once the state has been reset, the other ghosts can't kill pacman anymore
        -- this is because the state will be reset, and pacman does not overlap with any of the ghosts
        interactSingle g setGhost gs@GameState {pacman=pm, score=s} = case Pacman.interact pm g of
            Nothing -> pacmanDeath gs
            Just g  -> (setGhost g gs){score=s + ghostKillScore}

        interactAll = 
              interactSingle gb (\g _gs -> _gs{blinky=g})
            . interactSingle gp (\g _gs -> _gs{pinky=g})
            . interactSingle gi (\g _gs -> _gs{inky=g})
            . interactSingle gc (\g _gs -> _gs{clyde=g})

instance GridLocated PacMan where
    move = undefined
    getLocation (PacMan p _) = p

instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
