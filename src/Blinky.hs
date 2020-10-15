module Blinky where

import Model
import Graphics.Gloss

instance GridLocated Ghost where
    getLocation (Ghost p _ _ _ _) = p   

instance Sprite Ghost where 
    render (Ghost _ _ ghostName _ _) = getGhostColor ghostName $ Circle radius
        where 
            radius = minimum [cellWidth, cellHeight] / 2
            getGhostColor :: GhostName -> (Picture -> Picture)
            getGhostColor Blinky = Color red
            getGhostColor Pinky  = Color rose
            getGhostColor Inky   = Color aquamarine
            getGhostColor Clyde  = Color orange