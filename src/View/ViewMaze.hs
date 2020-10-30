module ViewMaze where

import Graphics.Gloss

import Model
import ModelMaze


instance Sprite Field where 
    render Wall         = renderWall
    render Dot          = renderDot
    render Pellet       = renderPellet
    render GhostHouse   = renderGhostHouse
    render _            = Blank

renderWall :: Picture
renderWall = Color blue $ rectangleSolid cellWidth cellHeight

renderDot :: Picture
renderDot = Color orange $ (circleSolid) (cellRadius * 0.3)

renderPellet :: Picture
renderPellet = Color orange $ (circleSolid) (cellRadius * 0.6)
        
renderGhostHouse :: Picture
renderGhostHouse = Color white $ (circleSolid) (cellRadius * 0.8)

