module ModelPacMan where

import ModelBase

data PacMan = PacMan {
    ppos   :: ModelBase.Point,
    pdir   :: Direction
}
    deriving (Show)

maxLives :: Int
maxLives = 3