module ViewGhost where

import Model
import Ghosts
import Graphics.Gloss

instance Sprite Ghost where 
    render (Ghost _ _ ghostName _ _) = getGhostColor ghostName $ circleSolid cellRadius
        where 
            getGhostColor :: GhostName -> (Picture -> Picture)
            getGhostColor Blinky = Color red
            getGhostColor Pinky  = Color rose
            getGhostColor Inky   = Color aquamarine
            getGhostColor Clyde  = Color orange