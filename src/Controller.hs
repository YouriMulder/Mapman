module Controller where

import Model
import Ghosts

import System.Random

{- debugging stuff -}
import Maze
{- /debugging stuff -}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gstate = do
    gen <- newStdGen
    gstateGhosts <- return $ updateGhosts gstate $ randomPos gen
    return gstateGhosts

    where -- only needed for scared/player controlled ghosts, others are already handled in the updateGhosts function:
          randPoint :: StdGen -> Model.Point
          randPoint seed = let (x, ySeed) = randomR (0, mazeWidth) seed in (Point x $ fst $ randomR (0, mazeHeight) ySeed) 

          randomPos :: StdGen -> Ghost -> Maybe Model.Point
          randomPos seed Ghost{gstate=(Scared _)}                 = Just $ randPoint seed

          -- todo: bij keypresses de direction van de bestuurde ghost aanpassen

-- step :: Float -> GameState -> IO GameState
-- step secs = return


input :: Event -> GameState -> IO GameState
input keyEvent@EventKey{} gstate
    = return (eventKeyHandler keyEvent gstate)
input _ gstate = return gstate

eventKeyHandler :: Event -> GameState -> GameState
eventKeyHandler (EventKey (Char c) _ _ _) gstate
    = gstate
eventKeyHandler _ gstate
    = gstate