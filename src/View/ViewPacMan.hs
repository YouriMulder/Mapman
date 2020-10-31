module ViewPacMan where

import Graphics.Gloss ( circleSolid, Picture(Color), yellow )


import Model ( Sprite(..) )
import ModelPacMan ( PacMan )
import ModelMaze ( cellRadius )
    
-- | The sprite instance for PacMan used to render PacMan.
instance Sprite PacMan where 
    render _ = Color yellow $ circleSolid cellRadius
