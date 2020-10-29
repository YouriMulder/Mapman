{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game 
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

windowWidth  :: Float
windowWidth  = mazeWidth

windowHeight :: Float
windowHeight = windowTopPadding + mazeHeight + windowBotPadding

windowTopPadding :: Float
windowTopPadding = 20

windowBotPadding :: Float
windowBotPadding = 20

mazeWidth :: Float
mazeWidth = 400

mazeHeight :: Float 
mazeHeight = 400

{- BASE DATA -}

-- default point datatype
data Point = Point Int Int
    deriving (Generic, ToJSON, FromJSON, Ord, Eq, Show)

-- order is needed for determining ghost move decision
data Direction = North | West | South | East
    deriving (Generic, ToJSON, FromJSON, Eq, Ord, Show, Enum)

directions :: [Direction]
directions = [North, West, South, East]

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West = East
opposite East = West

{- MAZE DATA -}
mazeAmountOfCellsWidth :: Int
mazeAmountOfCellsWidth = 28

mazeAmountOfCellsHeight :: Int
mazeAmountOfCellsHeight = 31

cellWidth :: Float
cellWidth = mazeWidth / fromIntegral mazeAmountOfCellsWidth

cellHeight :: Float
cellHeight = mazeHeight / fromIntegral mazeAmountOfCellsHeight

cellDiameter :: Float
cellDiameter = minimum [cellWidth, cellHeight]

cellRadius :: Float        
cellRadius = cellDiameter / 2


dist :: Model.Point -> Model.Point -> Int
-- Euclidian distance
dist (Point x y) (Point u v) = (x - u) ^ 2 + (y - v)^2

moveFrom :: Model.Point -> Direction -> Model.Point
moveFrom (Point x y) North = Point x $ (y - 1) `mod` mazeAmountOfCellsHeight
moveFrom (Point x y) South = Point x $ (y + 1) `mod` mazeAmountOfCellsHeight
moveFrom (Point x y) East  = Point ((x + 1) `mod` mazeAmountOfCellsWidth) y
moveFrom (Point x y) West  = Point ((x - 1) `mod` mazeAmountOfCellsWidth) y

-- info on what is on the ground in a certain location
data Field = Empty
           | Wall
           | Dot
           | Pellet
           | PacmanStart
           | GhostHouse
        deriving (Generic, ToJSON, FromJSON, Eq, Show, Enum)

type Maze  = M.Map Model.Point Field

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

data PacMan = PacMan {
    ppos   :: Model.Point,
    pdir   :: Direction
}
    deriving (Show)

data Ghost  = Ghost {
    gpos     :: Model.Point,
    gdir     :: Direction,
    gname    :: GhostName,
    gcontrol :: GhostControl,
    gstate   :: GhostState
}
    deriving (Show)

-- default type class for sprites (PacMan and Ghost will inherit these)
class GridLocated a where 
    move   :: a -> Model.Point -> Maze -> Model.Point  -- First point is a target. For Pacman, this target will be ignored
    getLocation :: a -> Model.Point
    setLocation :: a -> Model.Point -> a
    
    moveDirection :: a -> Direction -> a
    moveDirection a direction = setLocation a (moveFrom (getLocation a) direction)

class Sprite s where
    render :: s -> Picture

class Controllable c where
    setDirection :: c -> Direction -> c

maxLives :: Int
maxLives = 3

ghostKillScore :: Int
ghostKillScore = 100

dotScore :: Int
dotScore = 10

palletScore :: Int
palletScore = 50

data Pause = IsPaused | NotPaused
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
    paused    :: Pause,
    keysPressed :: S.Set Key,
    initialMaze :: Maze
}

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

setGameStatePacMan :: PacMan -> GameState -> GameState
setGameStatePacMan pacman gstate 
    = gstate { pacman = pacman }