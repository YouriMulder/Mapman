module Pacman where

import Model
import Graphics.Gloss

instance GridLocated PacMan where
    getLocation (PacMan p _ _) = p   

instance Sprite PacMan where 
    render _ = Color yellow $ Circle (diameter/2)
        where diameter = minimum [cellWidth, cellHeight]