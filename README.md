# frogue

A small roguelike I'm making to learn F#.

Current to-dos:

- Let pathfinding take non-unit movement costs, e.g. 2 turns to move through closed door
- Basic combat, one-hit kills (start with player invulnerable, so I can delay handling player death)
- Add controller information in status subjects/objects if several actors controlled by current player, or several players
- Add some more interesting tilesets
  - Let tile char depend on tile's neighbours, e.g. doors/walls depend on nearby doors/walls
    - Consolas supports "pipe" walls (U+2580 to U+256c), so can do Rogue-style walls and directional door chars
  - Anything else is best delayed until I'm using pixels
- ? Store status messages received/shown in log
- ? Let pathfinding route around stationary actors (-> one enemy can be kept behind a door forever if other route)
- ? Move output (i.e. status messages and map updates) into async buffer / event system (e.g. so engine can keep running turns while waiting for receiver key input)
- ? Add key configuration
