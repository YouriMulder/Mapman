module Main where

import Model
import Maze
import Ghosts
import Controller
import View

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game

file = "level.mm"

main :: IO()
main = do 
    contents <- readFile file

    let maze   = stringToMaze contents
    let pacMan = PacMan (find PacmanStart maze) West
    let pinky  = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Pinky  Computer (Scatter 100)
    let inky   = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Inky   Computer (Scatter 100)
    let blinky = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Blinky Computer (Scatter 100)
    let clyde  = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Clyde  (Player West) (Scatter 100)
    putStr "Starting debug checks\n"
    putStr "Is maze valid: "
    print $ validMaze maze
    
    putStr "Done with debug checks\n"

    let gameState = GameState {
        maze=           maze,
        pacman=         pacMan,
        pinky=          pinky,
        inky=           inky,
        blinky=         blinky,
        clyde=          clyde,
        score=          0,
        highScore=      0,
        lives=          maxLives,
        paused=         NotPaused,
        keysPressed=    S.empty 
    }
    playIO (InWindow "MapMan" (round windowWidth, round windowHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              gameState        -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

    