namespace Frogue
module Types =
    type Position = {
       X: int;
       Y: int;
    }

    type ActorTile =
    | PlayerTile
    | OrcTile
    | UnknownActorTile

    type MapTile =
    | EmptyTile
    | OpenDoorTile
    | ClosedDoorTile
    | WallTile
    | UnknownMapTile

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
        Tile: ActorTile
        Controller: Controller
        Name: ActorName
        Script: AIScript
    }

    type Map = {
        Width: int
        Height: int
        Tiles: MapTile list list
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
        | MindSwapActorAction of int * Controller
        | AttackAction of int * Actor
        | WaitAction

    type BlockedAction =
        | MoveActionBlockedByAlly
        | MoveActionBlockedByWall
        | MoveActionBlockedByVoid
        | OpenToActionBlockedByVoid
        | OpenToActionBlockedByInvalidTile
        | CloseToActionBlockedByVoid
        | CloseToActionBlockedByInvalidTile
        | CloseToActionBlockedByActor
        | MindSwapToActionBlockedByVoid
        | MindSwapToActionBlockedByNoActor
        | MindSwapToActionOnControlledActor

    type IncompleteAction =
        | OpenAction
        | CloseAction
        | MindSwapAction

    type Action =
        | CompletePlayerAction of CompletePlayerAction
        | CompleteAnyoneAction of CompleteAnyoneAction
        | BlockedAction of BlockedAction
        | IncompleteAction of IncompleteAction

    type Tileset =
    | DefaultTileset
    | DottedTileset

    type WorldState = {
        Actors: Actor list
        Map: Map
        Action: Action
    }

    type OutputState = {
        StatusBar: TextBox
        StatusBuffer: StatusBuffer
        Tileset: Tileset
    }
