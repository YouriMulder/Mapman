module ViewPacMan where

import Graphics.Gloss


import Model
import ModelPacMan
import ModelMaze
    

instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
