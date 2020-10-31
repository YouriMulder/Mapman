{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module ModelBase where

import GHC.Generics
import Data.Aeson

{- BASE DATA -}

-- | Default point datatype
data Point = Point Int Int
    deriving (Generic, ToJSON, FromJSON, Ord, Eq, Show)

-- | Order is needed for determining ghost move decision
data Direction = North | West | South | East
    deriving (Generic, ToJSON, FromJSON, Eq, Ord, Show, Enum)

-- | Returns all the Direcions possible.
directions :: [Direction]
directions = [North, West, South, East]

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West = East
opposite East = West