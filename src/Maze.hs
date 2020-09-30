module Maze where

import Model

import qualified Data.Map as M
import qualified Data.Set as S

-- todo: match on constant for maze width
readLine :: Int -> String -> [(Point, Field)]
readLine y s = readLine' 0 s
        where   readLine' :: Int -> String -> [(Point, Field)]
                readLine' x []        = [((Point dx y), Wall) | dx <- [x..mazeWidth - 1]]  -- if line is empty, fill with walls
                readLine' 28 _        = []                                             -- full maze width reached
                readLine' x (c:cs)    = ((Point x y), tile c):readLine' (x + 1) cs

                tile c = case c of 
                                'e' -> Empty
                                ' ' -> Empty
                                'p' -> Palette
                                'f' -> Fruit
                                'P' -> PacmanStart
                                'G' -> GhostHouse
                                _   -> Wall  -- this will automatically handle the newline case for us


stringToMaze :: String -> Maze
stringToMaze content =  
        M.fromList . concat $ map (uncurry readLine) (zip [0..mazeHeight - 1] $ (lines content) ++ repeat [])  -- fill missing lines with []

-- count occurrences of a field within a maze
count :: Field -> Maze -> Int
count f = M.size . M.filter (\g -> g == f)

-- get all points with/without a certain field
-- mostly used for debugging
getAll :: Field -> Maze -> [Point]
getAll f m = M.keys $ M.filter (\g -> g == f) m

getRest :: Field -> Maze -> [Point]
getRest f m = M.keys $ M.filter (\g -> g /= f) m

-- find first occurrence of field in the maze (or raise error if not present)
find :: Field -> Maze -> Point
-- a map is not really used for this purpose, but this will mostly be used to find the starting position for pacman and the ghost house
find f = fst . M.findMin . M.filter (\g -> g == f)  -- first element is the key (point)

validMoves :: Point -> Maze -> [Point] 
 -- if point is not a valid point in the maze, return that it is a wall
validMoves p m = [q | q <- map (moveFrom p) directions, M.findWithDefault Wall q m /= Wall]

reachable :: Point -> Maze -> [Point]
reachable start m = S.toList $ fst $ traverse (S.empty, S.singleton start)
        where traverse :: (S.Set Point, S.Set Point) -> (S.Set Point, S.Set Point)
              traverse (done, searching) | null searching = (done, S.empty)
              traverse (done, searching)                  = 
                      let nxt = S.union done searching  -- updated found values
                      -- only search through values we have not found yet
                      in traverse (nxt, S.fromList [q | p <- S.toList searching, q <- validMoves p m, not $ S.member q nxt])

validMaze :: Maze -> Bool
validMaze m = mazeSize            m 
              && onePacmanStart   m
              && oneGhostHouse    m
              && traversable      m
              && (not . deadEnds) m

        where validPoints      = [(Point x y) | x <- [0..mazeWidth - 1], y <- [0..mazeHeight - 1]]
              mazeSize       m = M.size m == mazeWidth * mazeHeight && all (flip M.member m) validPoints  -- all points must be set
              onePacmanStart m = count PacmanStart m == 1
              oneGhostHouse  m = count GhostHouse  m == 1
              traversable    m = S.fromList (reachable (find PacmanStart m) m) == S.fromList (getRest Wall m)
              deadEnds       m = any (\p -> (length $ validMoves p m) < 2) validPoints


