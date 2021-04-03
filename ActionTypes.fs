namespace ActionTypes
open Types

type PlayerAction =
| HelpAction
| QuitAction
| CancelAction
| SaveGameAction
| ToggleTileSetAction

type AnyoneAction =
| MoveAction of Position * Position
| OpenDoorAction of Position
| CloseDoorAction of Position
| MindSwapActorAction of ActorID * Controller
| AttackAction of ActorID * Actor * Position
| WaitAction

type IncompleteAction =
| OpenAction
| CloseAction
| MindSwapAction

type Action =
| PlayerAction of PlayerAction
| AnyoneAction of AnyoneAction
