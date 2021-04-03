namespace Command
open Types

type IncompleteCommand =
| Open
| Close
| MindSwap

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

type CommandInput =
| IncompleteCommand of IncompleteCommand
| Command of Command
