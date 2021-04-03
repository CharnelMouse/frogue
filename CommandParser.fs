module CommandParser
open Types
open CombatMap
open Command

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

type ParsedCommand =
| BlockedAction of BlockedAction
| Action of Action

let resolveMoveCommand combatState direction =
    let currentActorID = combatState.ActorCombatQueue.Head
    let currentActor =
        combatState.Actors
        |> Map.find currentActorID
    let oldPos = Map.find currentActorID combatState.ActorCombatPositions
    let map = combatState.CombatMap
    let newPos = neighbour oldPos direction
    let blockingActorID =
        combatState.ActorCombatPositions
        |> Map.tryFindKey (fun _ p -> p = newPos)
    match blockingActorID with
    | Some id ->
        let controller = combatState.Actors.[id].Controller
        match controller, tryGetTileAt newPos map with
        | cont, Some _ when cont = currentActor.Controller ->
            BlockedAction MoveActionBlockedByAlly
        | _, Some _ ->
            Action (AnyoneAction (AttackAction (id, combatState.Actors.[id], combatState.ActorCombatPositions.[id])))
        | _, None ->
            BlockedAction MoveActionBlockedByVoid
    | None ->
        match tryGetTileAt newPos map with
        | Some WallTile -> BlockedAction MoveActionBlockedByWall
        | Some ClosedDoorTile -> Action (AnyoneAction (OpenDoorAction newPos))
        | Some _ -> Action (AnyoneAction (MoveAction (oldPos, newPos)))
        | None -> BlockedAction MoveActionBlockedByVoid

let private resolveOpenToCommand combatState direction =
    let currentActorID = combatState.ActorCombatQueue.Head
    let pos = Map.find currentActorID combatState.ActorCombatPositions
    let map = combatState.CombatMap
    let toPos = neighbour pos direction
    match tryGetTileAt toPos map with
    | Some ClosedDoorTile -> Action (AnyoneAction (OpenDoorAction toPos))
    | Some _ -> BlockedAction OpenToActionBlockedByInvalidTile
    | None -> BlockedAction OpenToActionBlockedByVoid

let private resolveCloseToCommand combatState direction =
    let currentActorID = combatState.ActorCombatQueue.Head
    let pos = Map.find currentActorID combatState.ActorCombatPositions
    let map = combatState.CombatMap
    let toPos = neighbour pos direction
    let blockingActor =
        combatState.ActorCombatPositions
        |> Map.exists (fun _ p -> p = neighbour pos direction)
    match tryGetTileAt toPos map, blockingActor with
    | _, true -> BlockedAction CloseToActionBlockedByActor
    | Some OpenDoorTile, false -> Action (AnyoneAction (CloseDoorAction toPos))
    | Some _, false -> BlockedAction CloseToActionBlockedByInvalidTile
    | None, false -> BlockedAction CloseToActionBlockedByVoid

let private resolveMindSwapToCommand combatState direction =
    let currentActorID = combatState.ActorCombatQueue.Head
    let actor =
        combatState.Actors
        |> Map.find currentActorID
    let pos = Map.find currentActorID combatState.ActorCombatPositions
    let map = combatState.CombatMap
    let toPos = neighbour pos direction
    match posIsOnMap toPos map with
    | false ->
        BlockedAction MindSwapToActionBlockedByVoid
    | true ->
        let maybeBlockingActorID =
            combatState.ActorCombatPositions
            |> Map.tryFindKey (fun _ p -> p = toPos)
        match maybeBlockingActorID with
        | None ->
            BlockedAction MindSwapToActionBlockedByNoActor
        | Some id ->
            let blockingActor =
                combatState.Actors
                |> Map.find id
            match blockingActor.Controller with
            | c when c = actor.Controller ->
                BlockedAction MindSwapToActionOnControlledActor
            | _ ->
                (id, actor.Controller)
                |> MindSwapActorAction
                |> AnyoneAction
                |> Action

let resolveCommand combatState command =
    match command with
    | Move direction ->
        resolveMoveCommand combatState direction
    | OpenTo direction ->
        resolveOpenToCommand combatState direction
    | CloseTo direction ->
        resolveCloseToCommand combatState direction
    | MindSwapTo direction ->
        resolveMindSwapToCommand combatState direction
    | Wait ->
        Action (AnyoneAction WaitAction)
    | Help ->
        Action (PlayerAction HelpAction)
    | Quit ->
        Action (PlayerAction QuitAction)
    | Cancel ->
        Action (PlayerAction CancelAction)
    | SaveGameCommand ->
        Action (PlayerAction SaveGameAction)
    | ToggleTilesetCommand ->
        Action (PlayerAction ToggleTileSetAction)
