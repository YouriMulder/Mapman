module Maze where

import Model

import qualified Data.Map as M
import qualified Data.Set as S

defaultMaze :: String
defaultMaze = unlines [
        "wwwwwwwwwwwwwwwwwwwwwwwwwwww",
        "wddddddddddddwwddddddddddddw",
        "wdwwwwdwwwwwdwwdwwwwwdwwwwdw",
        "wpwwwwdwwwwwdwwdwwwwwdwwwwpw",
        "wdwwwwdwwwwwdwwdwwwwwdwwwwdw",
        "wddddddddddddddddddddddddddw",
        "wdwwwwdwwdwwwwwwwwdwwdwwwwdw",
        "wdwwwwdwwdwwwwwwwwdwwdwwwwdw",
        "wddddddwwddddwwddddwwddddddw",
        "wwwwwwdwwwww ww wwwwwdwwwwww",
        "wwwwwwdwwwww ww wwwwwdwwwwww",
        "wwwwwwdww          wwdwwwwww",
        "wwwwwwdww wwwwGwww wwdwwwwww",
        "wwwwwwdww wwwwwwww wwdwwwwww",
        "      d   wwwwwwww   d      ",
        "wwwwwwdww wwwwwwww wwdwwwwww",
        "wwwwwwdww wwwwwwww wwdwwwwww",
        "wwwwwwdww          wwdwwwwww",
        "wwwwwwdww wwwwwwww wwdwwwwww",
        "wwwwwwdww wwwwwwww wwdwwwwww",
        "wddddddddddddwwddddddddddddw",
        "wdwwwwdwwwwwdwwdwwwwwdwwwwdw",
        "wdwwwwdwwwwwdwwdwwwwwdwwwwdw",
        "wpddwwdddddddPddddddddwwddpw",
        "wwwdwwdwwdwwwwwwwwdwwdwwdwww",
        "wwwdwwdwwdwwwwwwwwdwwdwwdwww",
        "wddddddwwddddwwddddwwddddddw",
        "wdwwwwwwwwwwdwwdwwwwwwwwwwdw",
        "wdwwwwwwwwwwdwwdwwwwwwwwwwdw",
        "wddddddddddddddddddddddddddw",
        "wwwwwwwwwwwwwwwwwwwwwwwwwwww"
        ]

-- todo: match on constant for maze width
readLine :: Int -> String -> [(Point, Field)]
readLine y = readLine' 0
        where   readLine' :: Int -> String -> [(Point, Field)]
                readLine' x []        = [((Point dx y), Wall) | dx <- [x..mazeAmountOfCellsWidth - 1]]  -- if line is empty, fill with walls
                readLine' 28 _        = []                                             -- full maze width reached
                readLine' x (c:cs)    = ((Point x y), tile c):readLine' (x + 1) cs

                tile 'e' = Empty
                tile ' ' = Empty
                tile 'p' = Pellet
                tile 'd' = Dot
                tile 'P' = PacmanStart
                tile 'G' = GhostHouse
                tile _   = Wall  -- this will automatically handle the newline case for us

stringToMaze :: String -> Maze
stringToMaze content =  
        M.fromList $ concatMap (uncurry readLine) (zip [0..mazeAmountOfCellsHeight - 1] $ lines content ++ repeat [])  -- fill missing lines with []

mazeToString :: Maze -> String
mazeToString m = unlines [
                map pointToChar [(Point x y) | x <- [0..mazeAmountOfCellsWidth - 1]] | y <- [0..mazeAmountOfCellsHeight - 1]
        ]
        where   pointToChar = fieldToChar . flip getField m
                fieldToChar :: Field -> Char
                -- basically the reverse of the above
                fieldToChar Empty       = ' '
                fieldToChar Pellet      = 'p'
                fieldToChar Dot         = 'd'
                fieldToChar PacmanStart = 'P'
                fieldToChar GhostHouse  = 'G'
                fieldToChar Wall        = 'w'  

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

deleteField :: Point -> Maze -> Maze
deleteField p = M.adjust (\_ -> Empty) p 

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
              where mazeSize       = M.size m == mazeAmountOfCellsWidth * mazeAmountOfCellsHeight && all (flip M.member m) validPoints  -- all points must be set
                    validPoints    = [(Point x y) | x <- [0..mazeAmountOfCellsWidth - 1], y <- [0..mazeAmountOfCellsHeight - 1]]
validMaze m | not onePacmanStart   = Just ("Expected one pacman starting position, got " ++ (show $ count PacmanStart m))
              where onePacmanStart = count PacmanStart m == 1
validMaze m | not oneGhostHouse    = Just ("Expected one ghost house, got " ++ (show $ count GhostHouse m))
              where oneGhostHouse  = count GhostHouse  m == 1
validMaze m | not traversable      = Just "Not every field in the maze is reachable"
              where traversable    = S.fromList (getRest Wall m) == S.insert (find GhostHouse m) (S.fromList (reachable (find PacmanStart m) m))
validMaze m | deadEnds             = Just "Maze has dead ends"
              where deadEnds       = any (\p -> length (validMoves p m) < 2 && getField p m /= GhostHouse) $ getRest Wall m
validMaze _                        = Nothing