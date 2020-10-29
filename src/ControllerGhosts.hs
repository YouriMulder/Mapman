module ControllerGhosts where

import Model

instance Controllable Ghost where
    setDirection g@Ghost{gcontrol=Computer}   _ = g
    setDirection g@Ghost{gcontrol=(Player _)} d = g{gcontrol=Player d}

setGhostsComputerControlled :: GameState -> GameState
setGhostsComputerControlled gstate = 
    mapGhosts gstate (\g -> g{gcontrol=Computer})

setGameStateGhostPlayer :: Ghost -> GameState -> GameState
setGameStateGhostPlayer g@Ghost{gname=Pinky} gstate = 
    gstate{pinky=(setGhostPlayer g)}
setGameStateGhostPlayer g@Ghost{gname=Blinky} gstate = 
    gstate{blinky=(setGhostPlayer g)}
setGameStateGhostPlayer g@Ghost{gname=Inky} gstate = 
    gstate{inky=(setGhostPlayer g)}
setGameStateGhostPlayer g@Ghost{gname=Clyde} gstate = 
    gstate{clyde=(setGhostPlayer g)}

setGhostPlayer :: Ghost -> Ghost
setGhostPlayer g = g{gcontrol=Player (gdir g)}

