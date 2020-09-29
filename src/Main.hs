module Main where

import Serial
import Model
import Controller
import View

import qualified Data.Map as M
import Graphics.Gloss.Interface.IO.Game

file = "level.mm"

main :: IO()
main = do 
    contents <- readFile file
    let mazeContent = stringToMaze contents
    let maze = M.fromList mazeContent
    let pacMan = PacMan (Model.Point 0 0) (0,0) Normal
    let g1 = Ghost (Model.Point 0 0) (0,0) Pinky Computer Scary
    let g2 = Ghost (Model.Point 0 0) (0,0) Inky Computer Scary
    let g3 = Ghost (Model.Point 0 0) (0,0) Blinky Computer Scary
    let g4 = Ghost (Model.Point 0 0) (0,0) Clyde Computer Scary

    let gameState = GameState maze pacMan g1 g2 g3 g4 0 0 10 False
    playIO (InWindow "MapMan" (windowWidth, windowHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              gameState        -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

    