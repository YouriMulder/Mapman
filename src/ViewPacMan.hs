module ViewPacMan where

import Model
import Graphics.Gloss

instance GridLocated PacMan where
    move = undefined
    getLocation (PacMan p _) = p
    setLocation (PacMan _ d) p = PacMan p d
    

instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
