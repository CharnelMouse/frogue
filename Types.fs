namespace Frogue
module Types =
    type Position = {
       X: int;
       Y: int;
    }
    
    type Player = {
        Position: Position;
    }

    type Map = {
        Tiles: string list
        Width: int
        Height: int
    }

    type TextBox = {
        Start: Position;
        Length: int;
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
        | SaveGameCommand
        | ToggleTilesetCommand
        | UnknownCommand

    type IncompleteCommand =
        | Open
        | Close

    type Command =
        | CompleteCommand of CompleteCommand
        | IncompleteCommand of IncompleteCommand

    type CompleteAction =
        | StartSession
        | StartSessionWithUnknownTileset
        | MoveAction of Position * Position
        | OpenDoorAction of Position
        | CloseDoorAction of Position
        | WaitAction
        | HelpAction
        | QuitAction
        | CancelAction
        | SaveGameAction
        | ToggleTileSetAction
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

    type InternalTile =
    | EmptyTile
    | OpenDoorTile
    | ClosedDoorTile
    | WallTile
    | PlayerTile
    | UnknownTile

    type TilesetParser = InternalTile -> char

    type Tileset =
    | DefaultTileset
    | DottedTileset

    type GameState = {
        Player: Player
        Map: Map
        StatusBar: TextBox
        Action: Action
        Tileset: Tileset
    }
