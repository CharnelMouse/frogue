# frogue

A small roguelike I'm making to learn F#.

Current to-dos:

- ? Store status messages received/shown in log
- Add controller information in status subjects/objects if several actors controlled by current player, or several players
- ? Add pathfinding route around stationary actors
- Add some more interesting tilesets
  - Let tile char depend on tile's neighbours, e.g. doors/walls depend on nearby doors/walls
    - Consolas supports "pipe" walls (U+2580 to U+256c), so can do Rogue-style walls and directional door chars
  - Anything else is best delayed until I'm using pixels
- Add key configuration
- ? Move output (i.e. status messages and map updates) into async buffer / event system (e.g. so engine can keep running turns while waiting for receiver key input)
