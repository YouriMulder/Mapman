module ViewPacMan where

import Model
import ControllerPacMan
import Graphics.Gloss

instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
