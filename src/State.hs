module State(
    updateScore,
    victoryCheck,
    resetState,
    gameOver
) where

import qualified Data.Set as S
import Model
import Maze

updateScore :: GameState -> GameState
updateScore gs@GameState{score=s, highScore=hs} | s > hs = gs{highScore=s}
updateScore gs                                           = gs

victoryCheck :: GameState -> GameState
victoryCheck gs@GameState{maze=m} | count Dot m == 0 && count Pellet m == 0 = gs{runState=Victory $ 3 * fps}
victoryCheck gs                                                             = gs

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

