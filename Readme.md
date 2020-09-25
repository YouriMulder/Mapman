# Mapman
Pacman clone

## Game state
```Haskell
data MapMan = MapMap {
  maze      :: Maze,
  pacman    :: PacMan,
  blinky    :: Ghost,
  pinky     :: Ghost,
  inky      :: Ghost,
  clyde     :: Ghost,
  score     :: Int,
  highScore :: Int,
  lives     :: Int
}
```

# The Maze
```Haskell
data Point = Point Float Float
type Maze  = Map Point Field
```

Inside the maze we store the fields as follows
```Haskell
data Field = Empty
           | Wall
           | Palette
           | Fruit
```

## Pacman and the ghosts
```Haskell
data PMState      = Normal | Powered 
data GhostState   = Normal | Scared
data GhostName    = Pinky | Inky | Blinky | Clyde
data GhostControl = Computer | Player

data PacMan     = PacMan Point PMState
data Ghost      = Ghost Point GhostName GhostControl GhostState 

class Sprite s where
  move :: s -> Point -> s  -- point is PacMan's position

instance Sprite PacMan where
  move = (IO stuff)

instance Sprite Ghost where
  move (Ghost _ Pinky  _ _) = (Pinky's moving strategy)
  move (Ghost _ Blinky _ _) = (Blinky's moving strategy)
  ...
```

# Design
## 2.4 Implementation of the Minimum Requirements
**Player** <br />
    The user controls Pacman using the arrow keys. <br />
**Enemies** <br />
    In the game are mulitple enemies. <br />
    The enemies are the ghosts named: blinky, pinky, inky andclyde. <br />
**Randomness** <br />
    Once all the pallets are eaten, the game will continue as an endless mode. <br />
    The pallets will be placed at a random time on a random location. <br />
**Animation** <br />
    When fruit is comsumed by Pacman the ghost will start to flicker. <br />
    A long as the animation is present the ghosts can be eaten by pacman. <br />
**Pause** <br />
    Pressing ESC will pause the game, the game will continue when ESC is pressed again. <br />
**Interaction with the file system** <br />
    The interaction with the file system will be implemented by storing the maze as a file. <br />
    The maze will be loaded on startup. <br />
    

## 2.5 Implementation of the optional Requirements
We are planning to implement the following optional requirements. <br />
**Custom levels** <br />
    - We will implement custom levels, the custom levels will be loaded from a file. <br />
    - The custom maze must be the same dimentions as the original maze. <br />
    - Fields which are left empty (space) will be filled with walls. <br />
    - If a row or column does not have a ending or starting wall then there must NOT be a wall on the opposite. <br />
    Empty           = e <br />
    Wall            = w <br />
    Palette         = p <br />
    Fruit           = f <br />
    Pacman location = P <br />
    Ghost house     = G <br />

**Use JSON to save the full game state.** <br />
    - We will implement a way to store the current gamestate to a file. <br />
    - We will implement a way to load a gamestate stored in a file. <br />

**Multiplayer** <br />
    - The red ghosts can optionally be controlled by another player using WASD as input.  <br />
