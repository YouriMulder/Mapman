{-# LANGUAGE OverloadedStrings #-}

module Main where

import Model
import Maze
import Ghosts
import Controller
import View
import Serial

import System.Directory (
        doesDirectoryExist,
        doesFileExist,
        createDirectory,
        getCurrentDirectory
    )
import Data.Aeson
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game

directory = "./data"
file      = directory ++ "/level.mm"

setupDataDirectory :: IO()
setupDataDirectory = do
    createDirectory directory
    putStrLn "Created data directory"

    writeFile file defaultMaze

main :: IO()
main = do 
    workingDirectory <- getCurrentDirectory

    putStrLn $ "Working directory: " ++ (show workingDirectory)

    directoryExists <- doesDirectoryExist directory
    if not directoryExists then
        setupDataDirectory
    else
        putStrLn "Found data directory"

    fileExists <- doesFileExist file
    if not fileExists then
        writeFile file defaultMaze
    else
        putStrLn "Found level.mm file"

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

    playIO (InWindow "MapMan" (windowWidth, windowHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              gameState        -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
