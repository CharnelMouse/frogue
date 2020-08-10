# frogue

A small roguelike I'm making to learn F#.

Current to-dos:

- Add some more interesting tilesets
  - Might need to let wall tile type take whether neightbours are walls/doors as elements
- Add key configuration
- Split input->action parser into 1. Determine action; 2. Take action and change game state
  - Currently doing 1. Determine Action; 2. Change other parts of game state; 3. Pass action into game state
- Add non-player actors (requires parser split: NPAs give action not command, so need module to act on passed action)
