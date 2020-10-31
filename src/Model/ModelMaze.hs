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

mazeWidth :: Float
mazeWidth = 400

mazeHeight :: Float 
mazeHeight = 400

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


dist :: ModelBase.Point -> ModelBase.Point -> Int
-- Euclidian distance
dist (Point x y) (Point u v) = (x - u) ^ 2 + (y - v)^2

moveFrom :: ModelBase.Point -> Direction -> ModelBase.Point
moveFrom (Point x y) North = Point x                                      $ (y - 1) `mod` mazeAmountOfCellsHeight
moveFrom (Point x y) South = Point x                                      $ (y + 1) `mod` mazeAmountOfCellsHeight
moveFrom (Point x y) East  = Point ((x + 1) `mod` mazeAmountOfCellsWidth)    y
moveFrom (Point x y) West  = Point ((x - 1) `mod` mazeAmountOfCellsWidth)    y
