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

type ControllerName = string

type ControllerType =
| Player
| AIController

type ControllerInfo = ControllerType

type ActorID = int

type ActorName = string

type AICombatScript =
| WaitScript
| StandGround
| DumbHunt

type Actor = {
    Tile: CombatActorTile
    ControllerName: ControllerName
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
    Receiver: ControllerName
    Stream: string
}

type StatusState = {
    StatusBar: TextBox
    StatusBuffer: StatusBuffer
}

type SessionStartResult =
| NormalStart
| StartWithUnknownTileset

type Tileset =
| DefaultTileset
| DottedTileset

type Relation =
| SameController
| Enemy

type CombatState = {
    Actors: Map<ActorID, Actor>
    ActorCombatQueue: ActorID list
    ActorCombatPositions: Map<ActorID, Position>
    CombatMap: CombatMap
    Controllers: Map<ControllerName, ControllerInfo>
    ControllerRelations: Map<ControllerName*ControllerName, Relation>
}

type GameState =
| Combat of CombatState
| Win of Actor list
