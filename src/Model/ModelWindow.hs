module ModelWindow where

import ModelMaze

-- | The window width in pixels.
windowWidth  :: Float
windowWidth  = mazeWidth

-- | The window height in pixels, The maze an the paddings combined.
windowHeight :: Float
windowHeight = windowTopPadding + mazeHeight + windowBotPadding

-- | The top padding in pixels. Used for HUD.
windowTopPadding :: Float
windowTopPadding = 20

-- | The bottom padding in pixels. Used for HUD.
windowBotPadding :: Float
windowBotPadding = 20
