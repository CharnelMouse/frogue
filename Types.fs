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
        | CloseTo of Direction
        | Wait
        | Help
        | Quit
        | Cancel
        | UnknownCommand

    type IncompleteCommand =
        | Open
        | Close

    type Command =
        | CompleteCommand of CompleteCommand
        | IncompleteCommand of IncompleteCommand

    type CompleteAction =
        | StartSession
        | MoveAction of Position * Position
        | OpenDoorAction of Position
        | CloseDoorAction of Position
        | WaitAction
        | HelpAction
        | QuitAction
        | CancelAction
        | UnknownAction

    type BlockedAction =
        | MoveActionBlockedByWall
        | MoveActionBlockedByVoid
        | OpenToActionBlockedByVoid
        | OpenToActionBlockedByInvalidTile
        | CloseToActionBlockedByVoid
        | CloseToActionBlockedByInvalidTile

    type IncompleteAction =
        | OpenAction
        | CloseAction

    type Action =
        | CompleteAction of CompleteAction
        | BlockedAction of BlockedAction
        | IncompleteAction of IncompleteAction

    type TextBox = {
        Start: Position;
        Length: int;
    }
