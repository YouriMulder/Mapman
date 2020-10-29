module State(
    resetState,
    gameOver
) where

import qualified Data.Set as S
import Model
import Maze



resetState :: GameState -> GameState
resetState gs@GameState{maze=m} = (mapGhosts gs resetGhost){pacman=resetPacMan}
    where 
        resetPacMan :: PacMan
        resetPacMan = PacMan (find PacmanStart m) East

        resetGhost :: Ghost -> Ghost
        resetGhost g = g{gpos=find GhostHouse m, gdir=ghostInitialLook m, gstate=Scatter 100}

gameOver :: GameState -> GameState
gameOver gs@GameState{score=s} = 
    let reset@GameState{highScore=hs, initialMaze=initialMaze} = resetState gs in 
        reset{
            maze =          initialMaze, 
            score=          0,
            highScore=      max hs s, 
            lives=          maxLives,
            runState=       GameOver (3 * fps),
            keysPressed=    S.empty
        }

