module Maze where

import Model

import qualified Data.Map as M
import qualified Data.Set as S

defaultMaze :: String
defaultMaze = unlines [
        "wwwwwwwwwwwwwwwwwwwwwwwwwwww",
        "wppppppppppppwwppppppppppppw",
        "wpwwwwpwwwwwpwwpwwwwwpwwwwpw",
        "wpwwwwpwwwwwpwwpwwwwwpwwwwpw",
        "wfwwwwpwwwwwpwwpwwwwwpwwwwfw",
        "wppppppppppppppppppppppppppw",
        "wpwwwwpwwpwwwwwwwwpwwpwwwwpw",
        "wpwwwwpwwpwwwwwwwwpwwpwwwwpw",
        "wppppppwwppppwwppppwwppppppw",
        "wwwwwwpwwwww ww wwwwwpwwwwww",
        "wwwwwwpwwwww ww wwwwwpwwwwww",
        "wwwwwwpww          wwpwwwwww",
        "wwwwwwpww wwwwGwww wwpwwwwww",
        "wwwwwwpww wwwwwwww wwpwwwwww",
        "      p   wwwwwwww   p      ",
        "wwwwwwpww wwwwwwww wwpwwwwww",
        "wwwwwwpww wwwwwwww wwpwwwwww",
        "wwwwwwpww          wwpwwwwww",
        "wwwwwwpww wwwwwwww wwpwwwwww",
        "wwwwwwpww wwwwwwww wwpwwwwww",
        "wppppppppppppwwppppppppppppw",
        "wpwwwwpwwwwwpwwpwwwwwpwwwwpw",
        "wpwwwwpwwwwwpwwpwwwwwpwwwwpw",
        "wfppwwppppppp Ppppppppwwppfw",
        "wwwpwwpwwpwwwwwwwwpwwpwwpwww",
        "wwwpwwpwwpwwwwwwwwpwwpwwpwww",
        "wppppppwwppppwwppppwwppppppw",
        "wpwwwwwwwwwwpwwpwwwwwwwwwwpw",
        "wpwwwwwwwwwwpwwpwwwwwwwwwwpw",
        "wppppppppppppppppppppppppppw",
        "wwwwwwwwwwwwwwwwwwwwwwwwwwww"
        ]

-- todo: match on constant for maze width
readLine :: Int -> String -> [(Point, Field)]
readLine y = readLine' 0
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
        M.fromList $ concatMap (uncurry readLine) (zip [0..mazeHeight - 1] $ lines content ++ repeat [])  -- fill missing lines with []

-- count occurrences of a field within a maze
count :: Field -> Maze -> Int
count f = M.size . M.filter (== f)

-- get all points with/without a certain field
-- mostly used for debugging
getAll :: Field -> Maze -> [Point]
getAll f m = M.keys $ M.filter (== f) m

getRest :: Field -> Maze -> [Point]
getRest f m = M.keys $ M.filter (/= f) m

getAllPairs :: Maze -> [(Point, Field)]
getAllPairs = M.toList

-- find first occurrence of field in the maze (or raise error if not present)
find :: Field -> Maze -> Point
-- a map is not really used for this purpose, but this will mostly be used to find the starting position for pacman and the ghost house
find f = fst . M.findMin . M.filter (== f)  -- first element is the key (point)

getField :: Point -> Maze -> Field
getField = M.findWithDefault Wall

-- Returns True if direction is possible, otherwise False
isValidDirection :: Direction -> Point -> Maze -> Bool
isValidDirection direction position maze = direction `elem` (validMoves position maze)

validMoves :: Point -> Maze -> [Direction] 
 -- if point is not a valid point in the maze, return that it is a wall
validMoves p m = [q | q <- directions, let f = getField (moveFrom p q) m in f /= Wall && f /= GhostHouse]

ghostInitialLook :: Maze -> Direction
-- ghost must initially look in a direction they can move in
-- if we do not do this, we risk the ghosts looking away, and forcing themselves through walls to get out of the ghost house
ghostInitialLook m = head $ validMoves (find GhostHouse m) m

reachable :: Point -> Maze -> [Point]
reachable start m = S.toList $ fst $ traverse (S.empty, S.singleton start)
        where traverse :: (S.Set Point, S.Set Point) -> (S.Set Point, S.Set Point)
              traverse (done, searching) | null searching = (done, S.empty)
              traverse (done, searching)                  = 
                      let nxt = S.union done searching  -- updated found values
                      -- only search through values we have not found yet
                      in traverse (nxt, S.fromList [moveFrom p d | p <- S.toList searching, d <- validMoves p m, not $ S.member (moveFrom p d) nxt])

-- return Nothing if maze is valid, otherwise, return a reason why it is not
validMaze :: Maze -> Maybe String
validMaze m | not mazeSize         = Just "Maze is not of the right size, expected "
              where mazeSize       = M.size m == mazeWidth * mazeHeight && all (flip M.member m) validPoints  -- all points must be set
                    validPoints    = [(Point x y) | x <- [0..mazeWidth - 1], y <- [0..mazeHeight - 1]]
validMaze m | not onePacmanStart   = Just ("Expected one pacman starting position, got " ++ (show $ count PacmanStart m))
              where onePacmanStart = count PacmanStart m == 1
validMaze m | not oneGhostHouse    = Just ("Expected one ghost house, got " ++ (show $ count GhostHouse m))
              where oneGhostHouse  = count GhostHouse  m == 1
validMaze m | not traversable      = Just "Not every field in the maze is reachable"
              where traversable    = S.fromList (getRest Wall m) == S.insert (find GhostHouse m) (S.fromList (reachable (find PacmanStart m) m))
validMaze m | deadEnds             = Just "Maze has dead ends"
              where deadEnds       = any (\p -> length (validMoves p m) < 2 && getField p m /= GhostHouse) $ getRest Wall m
validMaze _                        = Nothing
