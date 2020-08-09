namespace Frogue
module Types =
    type Position = {
       X: int;
       Y: int;
    }
    
    type Player = {
        Position: Position;
    }

    type Direction =
        | North
        | South
        | East
        | West

    type CompleteCommand =
        | Move of Direction
        | OpenTo of Direction
        | Wait
        | Help
        | Quit
        | Cancel
        | UnknownCommand

    type IncompleteCommand =
        | Open

    type Command =
        | CompleteCommand of CompleteCommand
        | IncompleteCommand of IncompleteCommand

    type CompleteAction =
        | StartSession
        | MoveAction of Position * Position
        | MoveActionBlockedByWall
        | MoveActionBlockedByVoid
        | OpenDoorAction of Position
        | OpenToActionBlockedByVoid
        | OpenToActionBlockedByInvalidTile
        | WaitAction
        | HelpAction
        | QuitAction
        | CancelAction
        | UnknownAction

    type IncompleteAction =
        | OpenAction

    type Action =
        | CompleteAction of CompleteAction
        | IncompleteAction of IncompleteAction

    type TextBox = {
        Start: Position;
        Length: int;
    }
