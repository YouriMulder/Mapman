{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module ModelBase where

import GHC.Generics
import Data.Aeson

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