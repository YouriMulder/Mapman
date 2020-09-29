module Model where

import qualified Data.Map as M

{- BASE DATA -}

-- default point datatype
data Point = Point Int Int
    deriving (Ord, Eq, Show)

data Direction = North | South | East | West
    deriving (Eq, Show, Enum)

directions = [North ..]


{- MAZE DATA -}
mazeWidth = 28
mazeHeight = 31

moveFrom :: Point -> Direction -> Point
moveFrom (Point x y) North = (Point x $ (y + 1) `mod` mazeHeight)
moveFrom (Point x y) South = (Point x $ (y - 1) `mod` mazeHeight)
moveFrom (Point x y) East  = (Point ((x + 1) `mod` mazeWidth) y)
moveFrom (Point x y) West  = (Point ((x - 1) `mod` mazeWidth) y)

-- info on what is on the ground in a certain location
data Field = Empty
           | Wall
           | Palette
           | Fruit
           | PacmanStart
           | GhostHouse
        deriving (Eq, Show, Enum)

type Maze  = M.Map Point Field

{- SPRITE DATA -}

data PMState      = Normal | Powered                -- Pac-Man's state

data GhostState   = Scary  | Scared                 -- Ghost's state
data GhostName    = Pinky | Inky | Blinky | Clyde
data GhostControl = Computer | Player

data PacMan = PacMan Point Direction PMState
data Ghost  = Ghost Point Direction GhostName GhostControl GhostState 

-- default type class for sprites (PacMan and Ghost will inherit these)
class Sprite s where
    move :: s -> Point -> s  -- point is PacMan's position

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