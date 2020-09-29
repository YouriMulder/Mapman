module Main where

import Maze
import Model
import Controller
import View

import Graphics.Gloss.Interface.IO.Game

import Data.Set as S

file = "level.mm"

main :: IO()
main = do 
    contents <- readFile file
    let maze = stringToMaze contents
    let pacMan = PacMan (Model.Point 0 0) West Normal
    let g1 = Ghost (Model.Point 0 0) North Pinky Computer Scary
    let g2 = Ghost (Model.Point 0 0) North Inky Computer Scary
    let g3 = Ghost (Model.Point 0 0) North Blinky Computer Scary
    let g4 = Ghost (Model.Point 0 0) North Clyde Computer Scary

    putStr "Starting debug checks\n"
    putStr "Is maze valid: "
    print $ validMaze maze
    putStr "Done with debug checks\n"

    let gameState = GameState maze pacMan g1 g2 g3 g4 0 0 10 False
    playIO (InWindow "MapMan" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              gameState        -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

    