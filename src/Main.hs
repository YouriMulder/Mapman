{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model
    ( GameState(GameState, maze, pacman, pinky, inky, blinky, clyde,
                score, highScore, lives, runState, keysPressed, initialMaze),
      RunState(Paused),
      fps )
import ModelBase ( Direction(West) )
import ModelPacMan ( PacMan(PacMan), maxLives )
import ModelGhost
    ( Ghost(Ghost),
      GhostControl(Computer),
      GhostName(Clyde, Pinky, Inky, Blinky) )
import ModelMaze ( Field(GhostHouse, PacmanStart) )
import ModelWindow ( windowWidth, windowHeight )

import Maze
    ( defaultMaze, stringToMaze, find, ghostInitialLook, validMaze )
import Controller ( step, input )
import ControllerGhost ( refreshScatter )
import View ( view )
import Serial ( levelFile, initializeSerial )

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game
    ( black, playIO, Display(InWindow) )

main :: IO()
main = do 
    initializeSerial

    contents <- readFile levelFile

    let m = stringToMaze contents

    let (maze, message) = case validMaze m of
                        Just reason -> (stringToMaze defaultMaze, "Maze invalid: " ++ reason ++ "\nLoading default...")
                        Nothing     -> (m, "Maze OK")
    
    putStrLn message
        
    let pacMan = PacMan (find PacmanStart maze) West
    let pinky  = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Pinky  Computer refreshScatter
    let inky   = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Inky   Computer refreshScatter
    let blinky = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Blinky Computer refreshScatter
    let clyde  = Ghost  (find GhostHouse  maze) (ghostInitialLook maze) Clyde  Computer refreshScatter

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
        runState=       Paused,
        keysPressed=    S.empty,
        initialMaze=    maze
    }

    playIO (InWindow "MapMan" (ceiling windowWidth, ceiling windowHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              fps              -- Frames per second
              gameState        -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
