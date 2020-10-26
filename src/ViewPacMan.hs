module ViewPacMan where

import Model
import Ghost
import Graphics.Gloss

instance GridLocated PacMan where
    move = undefined
    getLocation (PacMan p _) = p

instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
