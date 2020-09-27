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
    | AIController

    type ActorName = string

    type AIScript =
    | WaitScript
    | StandGround
    | DumbHunt

    type Actor = {
        Position: Position
        Tile: InternalTile
        Controller: Controller
        Name: ActorName
        Script: AIScript
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

    type StatusBuffer = {
        Receiver: Controller
        Stream: string
    }

    type Direction =
        | North
        | South
        | East
        | West

    type CompletePlayerAction =
        | StartSession
        | StartSessionWithUnknownTileset
        | HelpAction
        | QuitAction
        | CancelAction
        | SaveGameAction
        | ToggleTileSetAction
        | UnknownAction

    type CompleteAnyoneAction =
        | MoveAction of Position * Position
        | OpenDoorAction of Position
        | CloseDoorAction of Position
        | AttackAction of Position
        | WaitAction

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
        | CompletePlayerAction of CompletePlayerAction
        | CompleteAnyoneAction of CompleteAnyoneAction
        | BlockedAction of BlockedAction
        | IncompleteAction of IncompleteAction

    type Tileset =
    | DefaultTileset
    | DottedTileset

    type GameState = {
        Actors: Actor list
        Map: Map
        StatusBar: TextBox
        StatusBuffer: StatusBuffer
        Action: Action
        Tileset: Tileset
    }
