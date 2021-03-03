module CommandParser
open Types
open CombatMap
open Command

let resolveMoveCommand worldState direction =
    let oldPos = worldState.Actors.Head.Position
    let map = worldState.CombatMap
    let newPos = neighbour oldPos direction
    let actorIndex = List.tryFindIndex (fun x -> x.Position = newPos) worldState.Actors
    let controller =
        match actorIndex with
            | Some ind -> Some worldState.Actors.[ind].Controller
            | None -> None
    match (controller, tryGetTileAt newPos map) with
    | (Some cont, Some _) when cont = worldState.Actors.Head.Controller -> BlockedAction MoveActionBlockedByAlly
    | (Some _, Some _) -> CompleteAnyoneAction (AttackAction (actorIndex.Value, worldState.Actors.[actorIndex.Value]))
    | (None, Some WallTile) -> BlockedAction MoveActionBlockedByWall
    | (None, Some ClosedDoorTile) -> CompleteAnyoneAction (OpenDoorAction newPos)
    | (None, Some _) -> CompleteAnyoneAction (MoveAction  (oldPos, newPos))
    | (_, None) -> BlockedAction MoveActionBlockedByVoid

let private resolveOpenToCommand worldState direction =
    let pos = worldState.Actors.Head.Position
    let map = worldState.CombatMap
    let toPos = neighbour pos direction
    match tryGetTileAt toPos map with
    | Some ClosedDoorTile -> CompleteAnyoneAction (OpenDoorAction toPos)
    | Some _ -> BlockedAction OpenToActionBlockedByInvalidTile
    | None -> BlockedAction OpenToActionBlockedByVoid

let private resolveCloseToCommand worldState direction =
    let pos = worldState.Actors.Head.Position
    let map = worldState.CombatMap
    let toPos = neighbour pos direction
    let blockingActor = List.tryFind (fun x -> x.Position = neighbour pos direction) worldState.Actors
    match tryGetTileAt toPos map, blockingActor with
    | _, Some _ -> BlockedAction CloseToActionBlockedByActor
    | Some OpenDoorTile, None -> CompleteAnyoneAction (CloseDoorAction toPos)
    | Some _, None -> BlockedAction CloseToActionBlockedByInvalidTile
    | None, None -> BlockedAction CloseToActionBlockedByVoid

let private resolveMindSwapToCommand worldState direction =
    let actor = worldState.Actors.Head
    let pos = actor.Position
    let map = worldState.CombatMap
    let toPos = neighbour pos direction
    match posIsOnMap toPos map with
    | false -> BlockedAction MindSwapToActionBlockedByVoid
    | true ->
        match List.tryFindIndex (fun x -> x.Position = toPos) worldState.Actors with
        | None -> BlockedAction MindSwapToActionBlockedByNoActor
        | Some a when worldState.Actors.[a].Controller = actor.Controller -> BlockedAction MindSwapToActionOnControlledActor
        | Some a -> CompleteAnyoneAction (MindSwapActorAction (a, actor.Controller))

let resolveCommand worldState command =
    match command with
    | CompleteCommand (Move direction) -> resolveMoveCommand worldState direction
    | CompleteCommand (OpenTo direction) -> resolveOpenToCommand worldState direction
    | CompleteCommand (CloseTo direction) -> resolveCloseToCommand worldState direction
    | CompleteCommand (MindSwapTo direction) -> resolveMindSwapToCommand worldState direction
    | CompleteCommand Wait -> CompleteAnyoneAction WaitAction
    | CompleteCommand Help -> CompletePlayerAction HelpAction
    | CompleteCommand Quit -> CompletePlayerAction QuitAction
    | CompleteCommand Cancel -> CompletePlayerAction CancelAction
    | CompleteCommand SaveGameCommand -> CompletePlayerAction SaveGameAction
    | CompleteCommand ToggleTilesetCommand -> CompletePlayerAction ToggleTileSetAction
    | CompleteCommand UnknownCommand -> CompletePlayerAction UnknownAction
    | IncompleteCommand Open -> IncompleteAction OpenAction
    | IncompleteCommand Close -> IncompleteAction CloseAction
    | IncompleteCommand MindSwap -> IncompleteAction MindSwapAction
