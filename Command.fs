namespace Frogue
module Command =
    open Types

    type CompleteCommand =
        | Move of Direction
        | OpenTo of Direction
        | CloseTo of Direction
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

    type Command =
        | CompleteCommand of CompleteCommand
        | IncompleteCommand of IncompleteCommand
