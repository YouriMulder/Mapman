module Default where

import Data.Map

{- BASE DATA -}

-- default point datatype
data Point = Point Int Int
    deriving (Eq, Show)

type Direction = (Int, Int)


{- MAZE DATA -}

mazeWidth :: Int
mazeWidth = 28

mazeHeight :: Int
mazeHeight = 31

-- info on what is on the ground in a certain location
data Field = Empty
           | Wall
           | Palette
           | Fruit
           | PacmanStart
           | GhostHouse
        deriving (Show)

type Maze  = Map Point Field

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

data MapMan = MapMan {
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