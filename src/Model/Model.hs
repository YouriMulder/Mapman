{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game 
import qualified Data.Set as S
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

import ModelBase
import ModelGhost
import ModelPacMan
import ModelMaze
import ModelWindow

-- default type class for sprites (PacMan and Ghost will inherit these)
class GridLocated a where 
    move   :: a -> ModelBase.Point -> Maze -> ModelBase.Point  -- First point is a target. For Pacman, this target will be ignored
    getLocation :: a -> ModelBase.Point
    setLocation :: a -> ModelBase.Point -> a
    
    moveDirection :: a -> Direction -> a
    moveDirection a direction = setLocation a (moveFrom (getLocation a) direction)

class Sprite s where
    render :: s -> Picture

class Controllable c where
    setDirection :: c -> Direction -> c

dotScore :: Int
dotScore = 10

palletScore :: Int
palletScore = 50

fps :: Int
fps = 10

data RunState = Normal
              | Paused
              | Death Int  -- countdown
              | GameOver Int
    deriving (Generic, ToJSON, FromJSON, Eq, Show)

data GameState = GameOverGameState | GameState {
    maze      :: Maze,
    pacman    :: PacMan,
    blinky    :: Ghost,
    pinky     :: Ghost,
    inky      :: Ghost,
    clyde     :: Ghost,
    score     :: Int,
    highScore :: Int,
    lives     :: Int,
    runState  :: RunState,
    keysPressed :: S.Set Key,
    initialMaze :: Maze
}

allGhosts :: GameState -> [Ghost]
allGhosts GameState{
    blinky = gb,
    pinky  = gp,
    inky   = gi,
    clyde  = gc
} = [gb, gp, gi, gc]

playerControlledGhosts :: GameState -> [Ghost]
playerControlledGhosts gstate = filter (not . isComputerGhost) ghosts
    where
        ghosts = allGhosts gstate
        isComputerGhost :: Ghost -> Bool
        isComputerGhost Ghost{gcontrol=Computer} = True
        isComputerGhost _                        = False

mapGhosts :: GameState -> (Ghost -> Ghost) -> GameState
mapGhosts gs@GameState{
    blinky = gb,
    pinky  = gp,
    inky   = gi,
    clyde  = gc
} f = gs{
    blinky = f gb,
    pinky  = f gp,
    inky   = f gi,
    clyde  = f gc
}