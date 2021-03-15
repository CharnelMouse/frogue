module CommandParser
open Types
open CombatMap
open Command

let resolveMoveCommand worldState direction =
    let currentActorID = worldState.ActorCombatQueue.Head
    let currentActor =
        worldState.Actors
        |> Map.find currentActorID
    let oldPos = Map.find currentActorID worldState.ActorPositions
    let map = worldState.CombatMap
    let newPos = neighbour oldPos direction
    let blockingActorID =
        worldState.ActorPositions
        |> Map.tryFindKey (fun _ p -> p = newPos)
    match blockingActorID with
    | Some id ->
        let controller = worldState.Actors.[id].Controller
        match controller, tryGetTileAt newPos map with
        | cont, Some _ when cont = currentActor.Controller ->
            BlockedAction MoveActionBlockedByAlly
        | _, Some _ ->
            CompleteAnyoneAction (AttackAction (id, worldState.Actors.[id], worldState.ActorPositions.[id]))
        | _, None ->
            BlockedAction MoveActionBlockedByVoid
    | None ->
        match tryGetTileAt newPos map with
        | Some WallTile -> BlockedAction MoveActionBlockedByWall
        | Some ClosedDoorTile -> CompleteAnyoneAction (OpenDoorAction newPos)
        | Some _ -> CompleteAnyoneAction (MoveAction (oldPos, newPos))
        | None -> BlockedAction MoveActionBlockedByVoid

let private resolveOpenToCommand worldState direction =
    let currentActorID = worldState.ActorCombatQueue.Head
    let pos = Map.find currentActorID worldState.ActorPositions
    let map = worldState.CombatMap
    let toPos = neighbour pos direction
    match tryGetTileAt toPos map with
    | Some ClosedDoorTile -> CompleteAnyoneAction (OpenDoorAction toPos)
    | Some _ -> BlockedAction OpenToActionBlockedByInvalidTile
    | None -> BlockedAction OpenToActionBlockedByVoid

let private resolveCloseToCommand worldState direction =
    let currentActorID = worldState.ActorCombatQueue.Head
    let pos = Map.find currentActorID worldState.ActorPositions
    let map = worldState.CombatMap
    let toPos = neighbour pos direction
    let blockingActor =
        worldState.ActorPositions
        |> Map.exists (fun _ p -> p = neighbour pos direction)
    match tryGetTileAt toPos map, blockingActor with
    | _, true -> BlockedAction CloseToActionBlockedByActor
    | Some OpenDoorTile, false -> CompleteAnyoneAction (CloseDoorAction toPos)
    | Some _, false -> BlockedAction CloseToActionBlockedByInvalidTile
    | None, false -> BlockedAction CloseToActionBlockedByVoid

let private resolveMindSwapToCommand worldState direction =
    let currentActorID = worldState.ActorCombatQueue.Head
    let actor =
        worldState.Actors
        |> Map.find currentActorID
    let pos = Map.find currentActorID worldState.ActorPositions
    let map = worldState.CombatMap
    let toPos = neighbour pos direction
    match posIsOnMap toPos map with
    | false ->
        BlockedAction MindSwapToActionBlockedByVoid
    | true ->
        let maybeBlockingActorID =
            worldState.ActorPositions
            |> Map.tryFindKey (fun _ p -> p = toPos)
        match maybeBlockingActorID with
        | None ->
            BlockedAction MindSwapToActionBlockedByNoActor
        | Some id ->
            let blockingActor =
                worldState.Actors
                |> Map.find id
            match blockingActor.Controller with
            | c when c = actor.Controller ->
                BlockedAction MindSwapToActionOnControlledActor
            | _ ->
                (id, actor.Controller)
                |> MindSwapActorAction
                |> CompleteAnyoneAction

let resolveCommand worldState command =
    match command with
    | CompleteCommand (Move direction) ->
        resolveMoveCommand worldState direction
    | CompleteCommand (OpenTo direction) ->
        resolveOpenToCommand worldState direction
    | CompleteCommand (CloseTo direction) ->
        resolveCloseToCommand worldState direction
    | CompleteCommand (MindSwapTo direction) ->
        resolveMindSwapToCommand worldState direction
    | CompleteCommand Wait ->
        CompleteAnyoneAction WaitAction
    | CompleteCommand Help ->
        CompletePlayerAction HelpAction
    | CompleteCommand Quit ->
        CompletePlayerAction QuitAction
    | CompleteCommand Cancel ->
        CompletePlayerAction CancelAction
    | CompleteCommand SaveGameCommand ->
        CompletePlayerAction SaveGameAction
    | CompleteCommand ToggleTilesetCommand ->
        CompletePlayerAction ToggleTileSetAction
    | CompleteCommand UnknownCommand ->
        CompletePlayerAction UnknownAction
    | IncompleteCommand Open ->
        IncompleteAction OpenAction
    | IncompleteCommand Close ->
        IncompleteAction CloseAction
    | IncompleteCommand MindSwap ->
        IncompleteAction MindSwapAction
