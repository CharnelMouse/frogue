namespace Frogue
module Types =
    type Position = {
       X: int;
       Y: int;
    }
    
    type InternalTile =
    | EmptyTile
    | OpenDoorTile
    | ClosedDoorTile
    | WallTile
    | PlayerTile
    | OrcTile
    | UnknownTile

    type Controller =
    | Player
    | AI

    type ActorType =
    | Adventurer
    | Orc

    type Actor = {
        Position: Position
        Tile: InternalTile
        Controller: Controller
        Type: ActorType
    }

    type Map = {
        Width: int
        Height: int
        Tiles: InternalTile list list
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
        | MoveActionBlockedByActor
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

    type TilesetParser = InternalTile -> char

    type Tileset =
    | DefaultTileset
    | DottedTileset

    type GameState = {
        Actors: Actor list
        Map: Map
        StatusBar: TextBox
        StatusBuffer: string
        Action: Action
        Tileset: Tileset
    }
