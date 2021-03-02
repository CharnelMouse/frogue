namespace Types
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

type ActorID = int

type ActorName = string

type AIScript =
| WaitScript
| StandGround
| DumbHunt

type Actor = {
    ID: ActorID
    Position: Position
    Tile: ActorTile
    Controller: Controller
    Name: ActorName
    Script: AIScript
}

type CombatMap = {
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

type StatusState = {
    StatusBar: TextBox
    StatusBuffer: StatusBuffer
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
    CombatMap: CombatMap
}
