module Ghosts (
    ghostTarget
)where

import Model 

{-
Information on the strategies of the individual ghosts:
https://gameinternals.com/understanding-pac-man-ghost-behavior
-}

{-
Basic rule:
ghosts try to reach their target tile. What the target tile is depends on the ghost.
There are a few basic rules:
    - Ghosts cannot turn back, so if they enter a tile on the left, they cannot go back the same direction
        - EXCEPT: when they enter scatter mode, they are forced to turn around
    - Decisions as to what direction to go in at a cross-section are made as follows:
        - Determine the adjacent tile with the smallest Euclidian distance to the target
        - If only one of those exists, go there
        - If multiple exist, choose the first one in order (Up, Left, Down, Right)

In scatter mode, the targets are:
    - Pink  : top left
    - Red   : top right
    - Orange: bottom left
    - Blue  : bottom right

In chase mode, the targets are:
    - Red   : Pac-man's current position
    - Pink  : Pac-man's current position + 4 * the direction he is going in
                Ex: Pac-man is at (0, 0) moving towards (1, 0), then pinky targets (4, 0)
    - Orange: if Clyde is further than 8 tiles away from Pac-man:
                Pac-man's current position is the target
              else:
                His "scatter" corner is the target
    - Blue  : This target is a bit more complicated. It depends on Pink's target and Red's position
              Draw a vector from Red's position to Pink's target
              Double this vector, the result is the target
-}

ghostCorner :: GhostName -> Point
ghostCorner Pinky  = (Point 2 0)
ghostCorner Blinky = (Point (mazeWidth - 3) 0)
ghostCorner Clyde  = (Point 2 $ mazeHeight - 1)
ghostCorner Inky   = (Point (mazeWidth - 3) (mazeHeight - 1))


pinkyTarget :: Point -> Direction -> Point
-- pinky's movement is based on Pac-man's position and direction
pinkyTarget p d = moveMultiple p d 4
    where moveMultiple :: Point -> Direction -> Int -> Point
          moveMultiple (Point x y) North t = (Point x $ y + t)
          moveMultiple (Point x y) South t = (Point x $ y - t)
          moveMultiple (Point x y) East  t = (Point (x + t) y)
          moveMultiple (Point x y) West  t = (Point (x - t) y)

blinkyTarget :: Point -> Point
blinkyTarget = id  -- simply Pac-man's position

clydeTarget :: Point -> Point -> Point
-- based on Clyde's position and Pac-man's position
clydeTarget clyde@(Point x y) pacman@(Point u v) | (x - u)^2 + (y - v)^2 > 8^2 = pacman
clydeTarget _ _                                                                = ghostCorner Clyde

inkyTarget :: Point -> PacMan -> Point
-- based on Blinky's position and Pac-man
inkyTarget (Point x y) (PacMan p d _) = let (Point u v) = pinkyTarget p d  
                                        in (Point (x + 2 * (u - x)) (y + 2 * (v - y)))

ghostTarget :: Ghost -> PacMan -> Point
ghostTarget (Ghost _ _ name   Player _)      _                          = ghostCorner name  -- todo: IO stuff
ghostTarget (Ghost _ _ name   _ (Scatter _)) _                          = ghostCorner name
ghostTarget (Ghost _ _ Pinky  _ _)           PacMan{ppos=pos, pdir=dir} = pinkyTarget pos dir
ghostTarget (Ghost _ _ Blinky _ _)           PacMan{ppos=pos}           = blinkyTarget pos
ghostTarget (Ghost q _ Clyde  _ _)           PacMan{ppos=p}             = clydeTarget q p
ghostTarget (Ghost q _ Inky   _ _)           PacMan{}                   = undefined  --helemaal kak

instance Sprite Ghost where
    move = undefined
    render = undefined