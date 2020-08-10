# frogue

A small roguelike I'm making to learn F#.

Current to-dos:

- Add tileset as seperate module / file (i.e. allow output tiles to be different from tiles used in map object)
- Add key configuration
- Split input->action parser into 1. Determine action; 2. Take action and change game state
  - Currently doing 1. Determine Action; 2. Change other parts of game state; 3. Pass action into game state
- Add non-player actors (requires parser split: NPAs give action not command, so need module to act on passed action)
