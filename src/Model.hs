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

type Direction = (Int, Int)


{- MAZE DATA -}

mazeWidth :: Int
mazeWidth = 28

mazeHeight :: Int
mazeHeight = 31

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
        deriving (Show)

type Maze  = M.Map Model.Point Field

{- SPRITE DATA -}

data PMState      = Normal | Powered                -- Pac-Man's state

data GhostState   = Scary  | Scared                 -- Ghost's state
data GhostName    = Pinky | Inky | Blinky | Clyde
data GhostControl = Computer | Player

data PacMan = PacMan Model.Point Direction PMState

data Ghost  = Ghost Model.Point Direction GhostName GhostControl GhostState 

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