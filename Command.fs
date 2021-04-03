namespace Command
open Compass

type Command =
| Move of Direction
| OpenTo of Direction
| CloseTo of Direction
| MindSwapTo of Direction
| Wait
| Help
| Quit
| Cancel
| SaveGameCommand
| ToggleTilesetCommand
