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
import Data.Aeson.Encode.Pretty
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game

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
            "maze"      .= (lines $ mazeToString (maze gs)),
            "pacman"    .= toJSON (pacman gs),
            "blinky"    .= toJSON (blinky gs),
            "pinky"     .= toJSON (pinky gs),
            "inky"      .= toJSON (inky gs),
            "clyde"     .= toJSON (clyde gs),
            "score"     .= score gs,
            "highScore" .= highScore gs,
            "runState"  .= toJSON (runState gs),
            "lives"     .= lives gs,
            "initialMaze"      .= (lines $ mazeToString (initialMaze gs))
            -- we don't dump the keysPressed field
        ]

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \obj -> do 
        mazeString <- obj .: "maze"
        pacman     <- obj .: "pacman"   
        blinky     <- obj .: "blinky"   
        pinky      <- obj .: "pinky"    
        inky       <- obj .: "inky"     
        clyde      <- obj .: "clyde"    
        score      <- obj .: "score"    
        highScore  <- obj .: "highScore"
        runState   <- obj .: "runState"   
        lives      <- obj .: "lives"    
        initialMazeString <- obj .: "initialMaze"  
        return GameState{
            maze        = stringToMaze $ unlines mazeString,
            pacman      = pacman,
            blinky      = blinky,
            pinky       = pinky,
            inky        = inky,
            clyde       = clyde,
            score       = score,
            highScore   = highScore,
            lives       = lives,
            runState    = runState,
            keysPressed = S.empty,
            initialMaze = stringToMaze $ unlines initialMazeString
        }

directory = "./data"
levelFile      = directory ++ "/level.mm"

initializeSerial :: IO()
initializeSerial = do
    workingDirectory <- getCurrentDirectory

    putStrLn $ "Working directory: " ++ (show workingDirectory)

    directoryExists <- doesDirectoryExist directory
    if not directoryExists then
        do
            createDirectory directory
            writeFile levelFile defaultMaze
            putStrLn "Created data directory"
    else
        putStrLn "Found data directory"

    fileExists <- doesFileExist levelFile
    if not fileExists then
        do 
            writeFile levelFile defaultMaze
            putStrLn "Created new level.mm file"
    else
        putStrLn "Found level.mm file"


slotName :: Int -> String
-- generate the filename for a save state slot
slotName slot = directory ++ "/mapman" ++ show slot ++ ".json"

dumpState :: GameState -> Int -> IO()
-- dump gamestate to a savestate "slot" (mapman<slot>.json)
dumpState gs slot = do
    putStrLn $ "Dumping to save state slot " ++ show slot
    BS.writeFile (slotName slot) (encodePretty gs)

loadState :: Int -> IO (Maybe GameState)
-- load gamestate from a savestate "slot"
loadState slot = do
    let fileName = slotName slot

    fileExists <- doesFileExist fileName
    if not fileExists  then do
        putStrLn $ "Save state slot " ++ show slot ++ " does not exist"
        return Nothing
    else do
        putStrLn $ "Loaded save state from slot " ++ show slot
        content <- BS.readFile (slotName slot)
        return $ decode content

getSlotID :: Key -> Int
getSlotID (SpecialKey KeyF1) = 1
getSlotID (SpecialKey KeyF2) = 2
getSlotID (SpecialKey KeyF3) = 3
getSlotID (SpecialKey KeyF4) = 4
getSlotID (SpecialKey KeyF5) = 5
getSlotID (Char '1') = 1
getSlotID (Char '2') = 2
getSlotID (Char '3') = 3
getSlotID (Char '4') = 4
getSlotID (Char '5') = 5
getSlotID _          = error "Invalid save slot requested"

-- check if we need to dump/load the state
-- these actions require IO interaction, so we can't handle them in pure keyhandler functions anyway
-- that's why we check this here
checkDumpState :: GameState -> IO()
checkDumpState gstate | S.size dumpKeys > 0 = do
        dumpState gstate $ getSlotID $ S.findMin dumpKeys
    where
        dumpKeys = S.intersection (keysPressed gstate) (S.fromList $ map SpecialKey [KeyF1, KeyF2, KeyF3, KeyF4, KeyF5])
checkDumpState _                            = return ()

checkLoadState :: GameState -> IO GameState
checkLoadState gstate | S.size loadKeys > 0 =
     do
        let loadKey = S.findMin loadKeys
        loaded <- loadState $ getSlotID loadKey
        let state = case loaded of
                        Just newState -> newState
                        Nothing       -> gstate
        return state
    where loadKeys = S.intersection (keysPressed gstate) (S.fromList $ map Char "12345")
checkLoadState gstate                       = return gstate
