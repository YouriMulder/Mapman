{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module ModelGhost where

import GHC.Generics
import Data.Aeson

import ModelBase

{- SPRITE DATA -}
data GhostState   = Scatter Int                     -- Ghost's state.
                  | Scary   Int                     -- Ghosts are scattering for a certain number of seconds, then chasing for a certain number of seconds
                  | Scared  Int
                  | Dead
                deriving (Generic, ToJSON, FromJSON, Show)
data GhostName    = Pinky | Inky | Blinky | Clyde
                deriving (Generic, ToJSON, FromJSON, Show, Enum)
data GhostControl = Computer 
                  | Player Direction  -- holds direction pressed by the player
                deriving (Generic, ToJSON, FromJSON, Show)

data Ghost  = Ghost {
    gpos     :: ModelBase.Point,
    gdir     :: Direction,
    gname    :: GhostName,
    gcontrol :: GhostControl,
    gstate   :: GhostState
}
    deriving (Show)

ghostKillScore :: Int
ghostKillScore = 100