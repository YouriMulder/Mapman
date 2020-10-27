module ControllerGhosts where

import Model
import Ghosts

instance Controllable Ghost where
    setDirection g@Ghost{gcontrol=Computer}   _ = g
    setDirection g@Ghost{gcontrol=(Player _)} d = g{gcontrol=Player d}
