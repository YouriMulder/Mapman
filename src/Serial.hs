module Serial where

import Model

-- todo: match on constant for maze width
readLine :: Int -> String -> [(Point, Field)]
readLine y s = readLine' 0 s
        where   readLine' :: Int -> String -> [(Point, Field)]
                readLine' x []        = [((Point dx y), Wall) | dx <- [x..mazeWidth]]           -- if line is empty, fill with walls
                readLine' 28 _        = []                                                      -- full maze width reached
                readLine' x (c:cs)    = ((Point x y), tile c):readLine' (x + 1) cs

                tile c = case c of 
                                'e' -> Empty
                                ' ' -> Empty
                                'p' -> Palette
                                'f' -> Fruit
                                'P' -> PacmanStart
                                'G' -> GhostHouse
                                _   -> Wall  -- this will automatically handle the newline case for us


stringToMaze :: String -> [(Point, Field)]
stringToMaze content =  
        concat $ map (uncurry readLine) (zip [0..mazeHeight] $ (lines content) ++ repeat [])  -- fill missing lines with []
        
