{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module ModelMaze where

import qualified Data.Map as M
import GHC.Generics
import Data.Aeson

import ModelBase

-- info on what is on the ground in a certain location
data Field = Empty
           | Wall
           | Dot
           | Pellet
           | PacmanStart
           | GhostHouse
        deriving (Generic, ToJSON, FromJSON, Eq, Show, Enum)

{- MAZE DATA -}
type Maze  = M.Map ModelBase.Point Field

-- | The maze width in pixels.
mazeWidth :: Float
mazeWidth = 400

-- | The maze height in pixels.
mazeHeight :: Float 
mazeHeight = 400

-- | The maze width in cells.
mazeAmountOfCellsWidth :: Int
mazeAmountOfCellsWidth = 28

-- | The maze height in cells.
mazeAmountOfCellsHeight :: Int
mazeAmountOfCellsHeight = 31

-- | The width in pixels of a cell.
cellWidth :: Float
cellWidth = mazeWidth / fromIntegral mazeAmountOfCellsWidth

-- | The height in pixels of a cell.
cellHeight :: Float
cellHeight = mazeHeight / fromIntegral mazeAmountOfCellsHeight

-- | The minimum diameter in pixels of a cell.
cellDiameter :: Float
cellDiameter = minimum [cellWidth, cellHeight]

-- | The minimum radius in pixels of a cell.
cellRadius :: Float        
cellRadius = cellDiameter / 2


-- | The Euclidian distance between to points.
dist :: ModelBase.Point -> ModelBase.Point -> Int
dist (Point x y) (Point u v) = (x - u) ^ 2 + (y - v)^2

-- | The next position based on the direction. 
-- | This returns one step in a direction.
moveFrom :: ModelBase.Point -> Direction -> ModelBase.Point
moveFrom (Point x y) North = Point x                                      $ (y - 1) `mod` mazeAmountOfCellsHeight
moveFrom (Point x y) South = Point x                                      $ (y + 1) `mod` mazeAmountOfCellsHeight
moveFrom (Point x y) East  = Point ((x + 1) `mod` mazeAmountOfCellsWidth)    y
moveFrom (Point x y) West  = Point ((x - 1) `mod` mazeAmountOfCellsWidth)    y
