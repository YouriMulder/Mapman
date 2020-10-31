module ViewMaze where

import Graphics.Gloss
    ( circleSolid,
      rectangleSolid,
      Picture(Blank, Color),
      blue,
      orange,
      white )
import Model ( Sprite(..) )
import ModelMaze
    ( Field(GhostHouse, Wall, Dot, Pellet),
      cellWidth,
      cellHeight,
      cellRadius )

-- | The sprite instance for a Field used to render a Field.
instance Sprite Field where 
    render Wall         = renderWall
    render Dot          = renderDot
    render Pellet       = renderPellet
    render GhostHouse   = renderGhostHouse
    render _            = Blank

-- | Renders a wall.
renderWall :: Picture
renderWall = Color blue $ rectangleSolid cellWidth cellHeight

-- | Renders a Dot.
renderDot :: Picture
renderDot = Color orange $ circleSolid (cellRadius * 0.3)

-- | Renders a Pallet
renderPellet :: Picture
renderPellet = Color orange $ circleSolid (cellRadius * 0.6)

-- | Renders a GhostHouse    
renderGhostHouse :: Picture
renderGhostHouse = Color white $ circleSolid (cellRadius * 0.8)

