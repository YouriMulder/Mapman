module State(
    resetState,
    gameOver
) where

import Model
import Maze

resetState :: GameState -> GameState
resetState gs@GameState{
    maze=m,
    blinky=gb,
    pinky=gp,
    inky=gi,
    clyde=gc
} = gs{
    -- reset only part of the fields
    pacman=resetPacMan,
    blinky=resetGhost gb,
    pinky=resetGhost gp,
    inky=resetGhost gi,
    clyde=resetGhost gc
    }
    where 
        resetPacMan :: PacMan
        resetPacMan = PacMan (find PacmanStart m) East

        resetGhost :: Ghost -> Ghost
        resetGhost g = g{gpos=find GhostHouse m, gdir=ghostInitialLook m, gstate=Scatter 100}

-- todo: reset maze
gameOver :: GameState -> GameState
gameOver gs@GameState{score=s} = let
    reset@GameState{highScore=hs} = resetState gs
    in reset{highScore=max hs s}

