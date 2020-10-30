module ViewPacMan where

import Graphics.Gloss


import Model
import ModelPacMan
import ModelMaze

instance GridLocated PacMan where
    move = undefined
    getLocation (PacMan p _) = p
    setLocation (PacMan _ d) p = PacMan p d
    

instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
