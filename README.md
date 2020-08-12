# frogue

A small roguelike I'm making to learn F#.

Current to-dos:

- Handle void lookups with options instead of failures
- Add some more interesting tilesets
  - Let tile char depend on tile's neighbours, e.g. doors/walls depend on nearby doors/walls
    - Consolas supports "pipe" walls (U+2580 to U+256c), so can do Rogue-style walls and directional door chars
  - Anything else is best delayed until I'm using pixels
- Add key configuration
- Split input->action parser into 1. Determine action; 2. Take action and change game state
  - Currently doing 1. Determine Action; 2. Change other parts of game state; 3. Pass action into game state
- Add non-player actors (requires parser split: NPAs give action not command, so need module to act on passed action)
