{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module ModelGhost where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )

import ModelBase ( Direction, Point )

{- SPRITE DATA -}
-- | The current state of a ghost. Used to determine the behaviour of the Ghost.
data GhostState   = Scatter Int  -- Ghost's state.
                  | Scary   Int  -- Ghosts are scattering for a certain number of frames, then chasing for a certain number of frames
                  | Scared  Int
                  | Dead
                deriving (Generic, ToJSON, FromJSON, Show)

-- | The name of a ghost.
data GhostName    = Pinky | Inky | Blinky | Clyde
                deriving (Generic, ToJSON, FromJSON, Show, Enum)

-- | The current controlling entity of the Ghost.
data GhostControl = Computer 
                  | Player Direction  -- holds direction pressed by the player
                deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | The current state of a Ghost.
data Ghost  = Ghost {
    gpos     :: ModelBase.Point,  -- | The current cell in the grid.
    gdir     :: Direction,        -- | The direction the Ghost is going.
    gname    :: GhostName,        -- | The name of the Ghost.
    gcontrol :: GhostControl,     -- | The entity which is controlling the Ghost.
    gstate   :: GhostState        -- | The current state of the Ghost.
}
    deriving (Show)