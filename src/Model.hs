module Model where

import qualified Data.Map as M
import Graphics.Gloss.Data.Picture

windowWidth  :: Int
windowWidth  = 400

windowHeight :: Int
windowHeight = 400

{- BASE DATA -}

-- default point datatype
data Point = Point Int Int
    deriving (Ord, Eq, Show)

-- order is needed for determining ghost move decision
data Direction = North | West | South | East
    deriving (Eq, Ord, Show, Enum)

directions :: [Direction]
directions = [North ..]


{- MAZE DATA -}
mazeWidth :: Int
mazeWidth = 28

mazeHeight :: Int
mazeHeight = 31


dist :: Model.Point -> Model.Point -> Int
-- Euclidian distance
dist (Point x y) (Point u v) = (x - u) ^ 2 + (y - v)^2

moveFrom :: Model.Point -> Direction -> Model.Point
moveFrom (Point x y) North = Point x $ (y + 1) `mod` mazeHeight
moveFrom (Point x y) South = Point x $ (y - 1) `mod` mazeHeight
moveFrom (Point x y) East  = Point ((x + 1) `mod` mazeWidth) y
moveFrom (Point x y) West  = Point ((x - 1) `mod` mazeWidth) y
cellWidth :: Float
cellWidth = fromIntegral windowWidth / fromIntegral mazeWidth

cellHeight :: Float
cellHeight = fromIntegral windowHeight / fromIntegral mazeHeight

-- info on what is on the ground in a certain location
data Field = Empty
           | Wall
           | Palette
           | Fruit
           | PacmanStart
           | GhostHouse
        deriving (Eq, Show, Enum)

type Maze  = M.Map Model.Point Field

{- SPRITE DATA -}

data PMState      = Normal | Powered                -- Pac-Man's state

data GhostState   = Scatter Int                     -- Ghost's state.
                  | Scary   Int                     -- Ghosts are scattering for a certain number of seconds, then chasing for a certain number of seconds
                  | Scared  Int
                  | Dead
data GhostName    = Pinky | Inky | Blinky | Clyde
data GhostControl = Computer | Player

data PacMan = PacMan {
    ppos   :: Model.Point,
    pdir   :: Direction,
    pstate :: PMState
}

data Ghost  = Ghost {
    gpos     :: Model.Point,
    gdir     :: Direction,
    gname    :: GhostName,
    gcontrol :: GhostControl,
    gstate   :: GhostState
}

-- default type class for sprites (PacMan and Ghost will inherit these)
class GridLocated a where 
    getLocation :: a -> Model.Point

class (GridLocated s) => Sprite s where
    move :: s -> Model.Point -> s  -- point is PacMan's position
    render :: s -> Picture

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
    paused    :: Bool
}