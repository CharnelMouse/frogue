# frogue

A small roguelike I'm making to learn F#.

Current to-dos:

- Add some more interesting tilesets
  - Let tile char depend on tile's neighbours, e.g. doors/walls depend on nearby doors/walls
    - Consolas supports "pipe" walls (U+2580 to U+256c), so can do Rogue-style walls and directional door chars
  - Anything else is best delayed until I'm using pixels
- Add key configuration
- Add non-player actors (requires parser split: NPAs give action not command, so need module to act on passed action)
