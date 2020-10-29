{-# LANGUAGE OverloadedStrings #-}
-- we want to just write Text instances as strings

module Serial where

import Model 
import Maze

import System.Directory (
        doesDirectoryExist,
        doesFileExist,
        createDirectory,
        getCurrentDirectory
    )
import qualified Data.ByteString.Lazy as BS

import Data.Aeson
import qualified Data.Set as S

instance ToJSON PacMan where
    toJSON pm = 
        object [
            "pos" .= toJSON (ppos pm),
            "dir" .= toJSON (pdir pm)
        ]

instance FromJSON PacMan where
    parseJSON = withObject "PacMan" $ \obj -> do 
        pos <- obj .: "pos"
        dir <- obj .: "dir"
        return PacMan{ppos=pos, pdir=dir}

instance ToJSON Ghost where
    toJSON g = 
        object [   
            "pos"     .= toJSON (gpos g),
            "dir"     .= toJSON (gdir g),
            "name"    .= toJSON (gname g),
            "control" .= toJSON (gcontrol g),
            "state"   .= toJSON (gstate g)
        ]

instance FromJSON Ghost where
    parseJSON = withObject "Ghost" $ \obj -> do 
        pos     <- obj .: "pos"
        dir     <- obj .: "dir"
        name    <- obj .: "name"
        control <- obj .: "control"
        state   <- obj .: "state"
        return Ghost{
            gpos=pos,
            gdir=dir,
            gname=name,
            gcontrol=control,
            gstate=state
        }

instance ToJSON GameState where
    toJSON gs = object [
            "maze"      .= toJSON (maze gs),
            "pacman"    .= toJSON (pacman gs),
            "blinky"    .= toJSON (blinky gs),
            "pinky"     .= toJSON (pinky gs),
            "inky"      .= toJSON (inky gs),
            "clyde"     .= toJSON (clyde gs),
            "score"     .= score gs,
            "highScore" .= highScore gs,
            "lives"     .= lives gs,
            "paused"    .= toJSON (paused gs)
            -- we don't dump the keysPressed field
        ]

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \obj -> do 
        maze      <- obj .: "maze"
        pacman    <- obj .: "pacman"   
        blinky    <- obj .: "blinky"   
        pinky     <- obj .: "pinky"    
        inky      <- obj .: "inky"     
        clyde     <- obj .: "clyde"    
        score     <- obj .: "score"    
        highScore <- obj .: "highScore"
        lives     <- obj .: "lives"    
        paused    <- obj .: "paused"   
        return GameState{
            maze        = maze,
            pacman      = pacman,
            blinky      = blinky,
            pinky       = pinky,
            inky        = inky,
            clyde       = clyde,
            score       = score,
            highScore   = highScore,
            lives       = lives,
            paused      = paused,
            keysPressed = S.empty
        }

directory = "./data"
file      = directory ++ "/level.mm"

initializeSerial :: IO()
initializeSerial = do
    workingDirectory <- getCurrentDirectory

    putStrLn $ "Working directory: " ++ (show workingDirectory)

    directoryExists <- doesDirectoryExist directory
    if not directoryExists then
        do
            createDirectory directory
            writeFile file defaultMaze
            putStrLn "Created data directory"
    else
        putStrLn "Found data directory"

    fileExists <- doesFileExist file
    if not fileExists then
        do 
            writeFile file defaultMaze
            putStrLn "Created new level.mm file"
    else
        putStrLn "Found level.mm file"


slotName :: Int -> String
-- generate the filename for a save state slot
slotName slot = directory ++ "/mapman" ++ show slot ++ ".json"

dumpState :: GameState -> Int -> IO GameState
-- dump gamestate to a savestate "slot" (mapman<slot>.json)
dumpState gs slot = do
    BS.writeFile (slotName slot) (encode gs)
    return gs

loadState :: Int -> IO (Maybe GameState)
-- load gamestate from a savestate "slot"
loadState slot = do
    let fileName = slotName slot

    fileExists <- doesFileExist fileName
    if not fileExists  then do
        putStrLn $ "Save state slot " ++ show slot ++ " does not exist"
        return Nothing
    else do
        content <- BS.readFile (slotName slot)
        return $ decode content
