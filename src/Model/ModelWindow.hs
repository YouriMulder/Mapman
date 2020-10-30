module ModelWindow where

import ModelMaze

windowWidth  :: Float
windowWidth  = mazeWidth

windowHeight :: Float
windowHeight = windowTopPadding + mazeHeight + windowBotPadding

windowTopPadding :: Float
windowTopPadding = 20

windowBotPadding :: Float
windowBotPadding = 20
