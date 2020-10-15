module ViewMaze where

import Model
import Graphics.Gloss


instance Sprite Field where 
    render Wall         = renderWall
    render Palette      = renderPallette
    render Fruit        = renderFruit
    render GhostHouse   = renderGhostHouse
    render _            = Blank

renderWall :: Picture
renderWall = Color blue $ rectangleSolid cellWidth cellHeight

renderPallette :: Picture
renderPallette = Color orange $ (circleSolid) (cellRadius * 0.3)

renderFruit :: Picture
renderFruit = Color orange $ (circleSolid) (cellRadius * 0.6)
        
renderGhostHouse :: Picture
renderGhostHouse = Color white $ (circleSolid) (cellRadius * 0.8)

