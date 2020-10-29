{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model
import Maze
import Ghosts
import Controller
import View
import Serial

import Data.Aeson
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game

main :: IO()
main = do 
    initializeSerial

    contents <- readFile file

    let m = stringToMaze contents

    let (maze, message) = case validMaze m of
                        Just reason -> (stringToMaze defaultMaze, "Maze invalid: " ++ reason ++ "\nLoading default...")
                        Nothing     -> (m, "Maze OK")
    
    putStrLn message
        
    let pacMan = PacMan (find PacmanStart maze) West
    let pinky  = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Pinky  Computer (Scatter 100)
    let inky   = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Inky   Computer (Scatter 100)
    let blinky = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Blinky Computer (Scatter 100)
    let clyde  = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Clyde  (Player West) (Scatter 100)

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

    -- proof of concept:
    dumpState gameState 0

    loaded <- loadState 1
    let loadedGS = case loaded of
                    Just gs -> gs
                    Nothing -> gameState

    playIO (InWindow "MapMan" (windowWidth, windowHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              loadedGS         -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
