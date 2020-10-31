# MapMan
The game MapMan is a clone of PacMan. Gather all the dots and pellets in the maze.

Read the design document for more detailed description of how to create a maze, how to control the enities, and how the program is structured.

## Controls
- p = Pause the game

### PacMan
- Arrow keys = pacman movement

### Ghosts
#### Switch to player controller Ghost
- 0 = All ghosts are now player controlled
- 9 = Player controls Blinky
- 8 = Player controls Inky
- 7 = Player controls Pinky
- 6 = Player controls Clyde

#### How to move the Ghost
Be aware the Ghost still moves though the Grid when not pressing a single button. You can influence the ghosts decision making by pressing the controls.

w = Move Up    on the next intersection.
a = Move Left  on the next intersection.
s = Move Down  on the next intersection.
d = Move Right on the next intersection.

### HUD
On the top and bottom of the screen you can see the following inforamtion
- Top left     = current score
- Top right    = high score
- Bottom left  = PacMan current Lives
- Bottom right = None if no player2 is active, And the name of the ghost when a player2 is active.
