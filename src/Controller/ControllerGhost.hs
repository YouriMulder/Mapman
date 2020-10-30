module ControllerGhost where

import Model 
import ModelBase
import ModelGhost
import ModelPacMan
import ModelMaze
import Maze

import Data.List ( minimumBy )

instance Controllable Ghost where
    setDirection g@Ghost{gcontrol=Computer}   _ = g
    setDirection g@Ghost{gcontrol=(Player _)} d = g{gcontrol=Player d}

instance GridLocated Ghost where
    move = undefined
    getLocation Ghost{gpos=gp} = gp
    setLocation (Ghost _ gdir gname gcontrol gstate) position =
        Ghost position gdir gname gcontrol gstate

setGhostsComputerControlled :: GameState -> GameState
setGhostsComputerControlled gstate = 
    mapGhosts gstate (\g -> g{gcontrol=Computer})

setGameStateGhostPlayer :: Ghost -> GameState -> GameState
setGameStateGhostPlayer g@Ghost{gname=Pinky} gstate = 
    gstate{pinky=setGhostPlayer g}
setGameStateGhostPlayer g@Ghost{gname=Blinky} gstate = 
    gstate{blinky=setGhostPlayer g}
setGameStateGhostPlayer g@Ghost{gname=Inky} gstate = 
    gstate{inky=setGhostPlayer g}
setGameStateGhostPlayer g@Ghost{gname=Clyde} gstate = 
    gstate{clyde=setGhostPlayer g}

setGhostPlayer :: Ghost -> Ghost
setGhostPlayer g = g{gcontrol=Player (gdir g)}

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

In frightened mode, the target is a pseudorandom direction
-}

ghostCorner :: GhostName -> Point
ghostCorner Pinky  = Point 2                                0
ghostCorner Blinky = Point (mazeAmountOfCellsWidth - 3)     0
ghostCorner Clyde  = Point 2                                $  mazeAmountOfCellsHeight -    1
ghostCorner Inky   = Point (mazeAmountOfCellsWidth - 3)     (mazeAmountOfCellsHeight - 1)

pinkyTarget :: Point -> Direction -> Point
-- pinky's movement is based on Pac-man's position and direction
pinkyTarget p d = moveMultiple p d 4
    where moveMultiple :: Point -> Direction -> Int -> Point
          moveMultiple (Point x y) North t = Point x $ y + t
          moveMultiple (Point x y) South t = Point x $ y - t
          moveMultiple (Point x y) East  t = Point (x + t) y
          moveMultiple (Point x y) West  t = Point (x - t) y

blinkyTarget :: Point -> Point
blinkyTarget = id  -- simply Pac-man's position

clydeTarget :: Point -> Point -> Point
-- based on Clyde's position and Pac-man's position
clydeTarget (Point x y) pacman@(Point u v) | (x - u)^2 + (y - v)^2 > 8^2 = pacman
clydeTarget _ _                                                          = ghostCorner Clyde

inkyTarget :: Point -> PacMan -> Point
-- based on Blinky's position and Pac-man
inkyTarget (Point x y) (PacMan p d) = let (Point u v) = pinkyTarget p d  
                                        in Point (x + 2 * (u - x)) (y + 2 * (v - y))

ghostTarget :: Ghost -> PacMan -> Maybe Point -> Point
{- some ghosts need an "auxiliary point"
In the case of (in order of priority):
 - Dead ghosts:              this is the ghost house
 - Player controller ghosts: this is the position in the direction the player set with IO
 - Scared ghosts:            this is a random position adjacent to the ghost's position, as in scared mode, ghosts move pseudorandomly
 - Inky:                     this is Blinky's position
-}
-- "special" states
-- dead state overrules the fact that the ghost is controlled by a player
ghostTarget Ghost{gstate=Dead}                              _      (Just p)      = p
ghostTarget Ghost{gstate=Dead}                              _      _             = error "Unknown GhostHouse position for dead ghost"
ghostTarget Ghost{gpos=gp, gcontrol=(Player d)}             _      _             = moveFrom gp d
ghostTarget Ghost{gstate=(Scared _)}                        _      (Just p)      = p
ghostTarget Ghost{gstate=(Scared _)}                        _      _             = error "Unknown Random position for scared ghost"
ghostTarget Ghost{gname=name, gstate=(Scatter _)}           _      _             = ghostCorner name

-- "normal" states  
ghostTarget Ghost{gname=Pinky}          PacMan{ppos=pos, pdir=dir} _             = pinkyTarget pos dir
ghostTarget Ghost{gname=Blinky}         PacMan{ppos=pos}           _             = blinkyTarget pos
ghostTarget Ghost{gname=Clyde, gpos=gp} PacMan{ppos=p}             _             = clydeTarget gp p
ghostTarget Ghost{gname=Inky}           pm                         (Just blinky) = inkyTarget blinky pm
ghostTarget Ghost{gname=Inky}           _                          _             = error "Unknown Blinky position for Inky"

ghostDir :: Ghost -> Point -> Maze -> Direction
-- the general strategy in how ghosts move towards their target
-- ghosts are not allowed to move back into themselves
ghostDir Ghost{gpos=gp, gdir=gd} p m = case choices of 
    [] -> gd -- error "Invalid ghost position in ghostDir"
    _  -> minimumBy cmp choices
    where 
        choices = filter (/= opposite gd) $ validMoves gp m

        cmp d e = compare (ord d) (ord e)

        ord :: Direction -> (Int, Direction)
        ord d = (dist p $ moveFrom gp d, d)

refreshScatter :: GhostState
refreshScatter = Scatter $ 6 * fps

refreshScary :: GhostState
refreshScary = Scary $ 20 * fps

scare :: Ghost -> Ghost
scare g@Ghost{gstate=Dead} = g  -- can't scare a dead ghost
-- when a ghost gets scared, the direction he is moving in flips
scare g@Ghost{gdir=gd}     = g{gdir=opposite gd, gstate=Scared $ 8 * fps}

ghostMove :: Ghost -> PacMan -> Maybe Point -> Maze -> Ghost
ghostMove g@(Ghost gp _ n c s) pm p m = Ghost nextPos dir n c (nextState s)
    where dir     = ghostDir g (ghostTarget g pm p) m
          nextPos = moveFrom gp dir
        
          nextState :: GhostState -> GhostState
          nextState (Scary   1) = refreshScatter
          nextState (Scary   t) = Scary (t - 1)
          nextState (Scatter 1) = refreshScary
          nextState (Scatter t) = Scatter (t - 1)
          nextState (Scared  1) = refreshScatter
          nextState (Scared  t) = Scared  (t - 1)

          -- when the ghost is dead, the p parameter is the ghost house, as mentioned above
          nextState Dead        = case p of
              (Just gh) -> if dist nextPos gh == 1 
                           then refreshScary -- next to ghosthouse position
                           else Dead
              _         -> Dead 

updateGhosts :: (Ghost -> Maybe Point) -> GameState -> GameState
-- blinky is needed for inky's auxiliary position
updateGhosts rand gs@GameState{pacman=pm, maze=m, blinky=gb} = mapGhosts gs updateGhost
    where auxPos :: Ghost -> Maybe Point
          auxPos Ghost{gstate=Dead}         = Just $ find GhostHouse m
          auxPos g@Ghost{gstate=(Scared _)} = rand g
          auxPos Ghost{gname=Inky}          = Just $ gpos gb
          auxPos _                          = Nothing
          
          updateGhost :: Ghost -> Ghost
          updateGhost g = ghostMove g pm (auxPos g) m


