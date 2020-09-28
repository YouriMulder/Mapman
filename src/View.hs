module View where

import Model

import Graphics.Gloss
view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure _ = Circle 10