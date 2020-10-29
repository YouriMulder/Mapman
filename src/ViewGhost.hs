module ViewGhost where

import Model
import Ghosts
import Graphics.Gloss

instance Sprite Ghost where 
    render g = Color (getGhostColor g) $ circleSolid cellRadius
        where 
            getGhostColor :: Ghost -> Color
            getGhostColor   Ghost{gstate=(Scared n)} | even n = blue
            getGhostColor   Ghost{gstate=(Scared _)}          = white
            -- dead ghosts: same as the normal color, but darker
            getGhostColor g@Ghost{gstate=Dead}                = mixColors 0.95 0.05 black (getGhostColor g{gstate=Scary 1})
            getGhostColor   Ghost{gname=Blinky}               = dark red
            getGhostColor   Ghost{gname=Pinky}                = bright rose
            getGhostColor   Ghost{gname=Inky}                 = dark aquamarine
            getGhostColor   Ghost{gname=Clyde}                = orange