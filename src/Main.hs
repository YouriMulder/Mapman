module Main where

import Model
import Maze
import Ghosts
import Controller
import View

import Graphics.Gloss.Interface.IO.Game

file = "level.mm"

main :: IO()
main = do 
    contents <- readFile file

    let maze = stringToMaze contents
    let pacMan = PacMan (Model.Point 0 0) West Normal
    let g1 = Ghost (Model.Point 1 1) North Pinky Computer (Scatter 100)
    let g2 = Ghost (Model.Point 1 2) North Inky Computer (Scatter 100)
    let g3 = Ghost (Model.Point 2 10) North Blinky Computer (Scatter 100)
    let g4 = Ghost (Model.Point 11 11) North Clyde Computer (Scatter 100)

    putStr "Starting debug checks\n"
    putStr "Is maze valid: "
    print $ validMaze maze
    
    putStr "Done with debug checks\n"

    let gameState = GameState maze pacMan g1 g2 g3 g4 0 0 10 False
    playIO (InWindow "MapMan" (windowWidth, windowHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              gameState        -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

    