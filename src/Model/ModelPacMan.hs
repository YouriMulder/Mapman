module ModelPacMan where

import ModelBase

-- | Data type of the current state of PacMan.
data PacMan = PacMan {
    ppos   :: ModelBase.Point,  -- Current position in the grid.
    pdir   :: Direction         -- Current direction PacMan is facing.
}
    deriving (Show)

-- | The maximum amount of lives of PacMan.
maxLives :: Int
maxLives = 3