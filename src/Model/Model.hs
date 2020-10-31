{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import Graphics.Gloss.Data.Picture ( Picture )
import Graphics.Gloss.Interface.IO.Game ( Key ) 
import qualified Data.Set as S
import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

import ModelBase ( Direction, Point )
import ModelGhost ( Ghost(gcontrol), GhostControl(Computer) )
import ModelPacMan ( PacMan )
import ModelMaze ( Maze, moveFrom )

-- | Default type class for sprites (PacMan and Ghost will inherit these)
class GridLocated a where 
    getLocation :: a -> ModelBase.Point
    setLocation :: a -> ModelBase.Point -> a
    
    moveDirection :: a -> Direction -> a
    moveDirection a direction = setLocation a (moveFrom (getLocation a) direction)

-- | Type class for sprites used to render.
class Sprite s where
    render :: s -> Picture

-- | Type class for controllable sprites, PacMan of player controlled Ghost.
class Controllable c where
    setDirection :: c -> Direction -> c

-- | The score you gain when touching a Dot.
dotScore :: Int
dotScore = 10

-- | The score you gain when touching a Pallet.
palletScore :: Int
palletScore = 50

-- | The score you gain for killing a Ghost.
ghostKillScore :: Int
ghostKillScore = 100

-- | The amount of FPS the game is running at.
fps :: Int
fps = 10

-- | The current state of the game, Normal is running.
data RunState = Normal
              | Paused
              | Death Int  -- countdown
              | GameOver Int
              | Victory Int
    deriving (Generic, ToJSON, FromJSON, Eq, Show)

-- | The GameState which holds all the information for the current gamestate. To render and control the game.
data GameState = GameState {
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

-- | Returns all the ghosts using the GameState.
allGhosts :: GameState -> [Ghost]
allGhosts GameState{
    blinky = gb,
    pinky  = gp,
    inky   = gi,
    clyde  = gc
} = [gb, gp, gi, gc]

-- | Returns all the player controlled Ghosts. 
playerControlledGhosts :: GameState -> [Ghost]
playerControlledGhosts = filter isComputerGhost . allGhosts
    where
        isComputerGhost :: Ghost -> Bool
        isComputerGhost = (Computer /=) . gcontrol

-- | Applies a function to all the Ghosts in a GameState
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