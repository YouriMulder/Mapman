{-# LANGUAGE OverloadedStrings #-}
-- we want to just write Text instances as strings

module Serial where

import Model 
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

