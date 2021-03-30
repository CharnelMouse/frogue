namespace Types
type Position = {
   X: int;
   Y: int;
}

type CombatActorTile =
| PlayerTile
| OrcTile
| UnknownActorTile

type CombatMapTile =
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

type AICombatScript =
| WaitScript
| StandGround
| DumbHunt

type Actor = {
    Tile: CombatActorTile
    Controller: Controller
    Name: ActorName
    Script: AICombatScript
}

type CombatMap = {
    Width: int
    Height: int
    Tiles: Map<Position, CombatMapTile>
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
| MindSwapActorAction of ActorID * Controller
| AttackAction of ActorID * Actor * Position
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

type CombatState = {
    Actors: Map<ActorID, Actor>
    ActorCombatQueue: ActorID list
    ActorCombatPositions: Map<ActorID, Position>
    CombatMap: CombatMap
}

type GameState =
| Combat of CombatState
| Win of Actor list
