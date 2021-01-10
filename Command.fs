namespace Command
open Types

type CompleteCommand =
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
| UnknownCommand

type IncompleteCommand =
| Open
| Close
| MindSwap

type Command =
| CompleteCommand of CompleteCommand
| IncompleteCommand of IncompleteCommand
