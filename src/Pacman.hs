module Pacman where

import Model
import Graphics.Gloss

instance GridLocated PacMan where
    move = undefined
    getLocation (PacMan p _ _) = p

instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
