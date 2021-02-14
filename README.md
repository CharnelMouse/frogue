# frogue

A small roguelike I'm making to learn F#.

Current to-dos:

- Fix second status line not showing when all generated on player turn (e.g. help text)
- Add main menu as precursor to adding encounter scene
- Switch to separate processes for ouput, AI, etc.
  - Separate map screen updates from status box updates
  - Make Dijkstra module independent of map tile information
- Turn death checks into new event pushes, instead of handling during attack event
- Add controller information in status subjects/objects if several actors controlled by current player, or several players
- Add some more interesting tilesets
  - Let tile char depend on tile's neighbours, e.g. doors/walls depend on nearby doors/walls
    - Consolas supports "pipe" walls (U+2580 to U+256c), so can do Rogue-style walls and directional door chars
  - Anything else is best delayed until I'm using pixels
- ? Store status messages received/shown in log
- ? Let pathfinding route around stationary actors (-> one enemy can be kept behind a door forever if other route)
- ? Move output (i.e. status messages and map updates) into async buffer / event system (e.g. so engine can keep running turns while waiting for receiver key input)
- ? Add key configuration
