module Controller where

import Model
import Ghosts

{- debugging stuff -}
import Maze
{- /debugging stuff -}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gstate@GameState{blinky=Ghost{gstate=gs}, maze=m} = do 
    print $ gs
    return gstateGhosts

    where gstateGhosts = updateGhosts gstate

-- step :: Float -> GameState -> IO GameState
-- step secs = return


input :: Event -> GameState -> IO GameState
input keyEvent@EventKey{} gstate
    = return (eventKeyHandler keyEvent gstate)
input _ gstate = return gstate

eventKeyHandler :: Event -> GameState -> GameState
eventKeyHandler (EventKey (Char c) _ _ _) gstate
    = gstate
eventKeyHandler _ gstate
    = gstate