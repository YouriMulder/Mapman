module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gstate = return gstate

input :: Event -> GameState -> IO GameState
input keyEvent@(EventKey _ _ _ _) gstate
    = return (eventKeyHandler keyEvent gstate)
input _ gstate = return gstate

eventKeyHandler :: Event -> GameState -> GameState
eventKeyHandler (EventKey (Char c) _ _ _) gstate
    = gstate
eventKeyHandler _ gstate
    = gstate