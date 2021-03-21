module Action
open Types

let private replaceSingleElementFn index replacer list =
    List.mapi (fun i x -> if i = index then replacer x else x) list

let private changeMapTile map pos tile =
    {map with Tiles = Map.change pos (fun _ -> Some tile) map.Tiles}

let private changePlayerPosition combatState pos =
    let actorID = combatState.ActorCombatQueue.Head
    let newPositions =
        combatState.ActorCombatPositions
        |> Map.change actorID (Option.bind (fun _ -> Some pos))
    {combatState with ActorCombatPositions = newPositions}

let private changeMaybeActorController controller maybeActor =
    match maybeActor with
    | Some actor -> Some {actor with Controller = controller}
    | None -> None

let private changeActorController combatState id controller =
    let currentActorID = combatState.ActorCombatQueue.Head
    let {Controller = targetController} =
        combatState.Actors
        |> Map.find id
    let newActors =
        combatState.Actors
        |> Map.change currentActorID (changeMaybeActorController targetController)
        |> Map.change id (changeMaybeActorController controller)
    {combatState with Actors = newActors}

let private executeOpenDoorAction combatState pos =
    {combatState with
        CombatMap = changeMapTile combatState.CombatMap pos OpenDoorTile
    }

let private executeCloseDoorAction combatState pos =
    {combatState with
        CombatMap = changeMapTile combatState.CombatMap pos ClosedDoorTile
    }

let private removeActor combatState id =
    {combatState with
        Actors =
            combatState.Actors
            |> Map.remove id
        ActorCombatQueue =
            combatState.ActorCombatQueue
            |> List.filter (fun n -> n <> id)
        ActorCombatPositions =
            combatState.ActorCombatPositions
            |> Map.remove id
    }

let executeAction combatState action =
    match action with
    | CompleteAnyoneAction (OpenDoorAction toPos) ->
        executeOpenDoorAction combatState toPos
    | CompleteAnyoneAction (CloseDoorAction toPos) ->
        executeCloseDoorAction combatState toPos
    | CompleteAnyoneAction (MoveAction (_, newPos)) ->
        changePlayerPosition combatState newPos
    | CompleteAnyoneAction (MindSwapActorAction (index, controller)) ->
        changeActorController combatState index controller
    | CompleteAnyoneAction (AttackAction (id, _, _)) ->
        removeActor combatState id
    | BlockedAction MoveActionBlockedByAlly
    | BlockedAction MoveActionBlockedByVoid
    | BlockedAction MoveActionBlockedByWall
    | BlockedAction OpenToActionBlockedByVoid
    | BlockedAction OpenToActionBlockedByInvalidTile
    | BlockedAction CloseToActionBlockedByVoid
    | BlockedAction CloseToActionBlockedByInvalidTile
    | BlockedAction CloseToActionBlockedByActor
    | BlockedAction MindSwapToActionBlockedByVoid
    | BlockedAction MindSwapToActionBlockedByNoActor
    | BlockedAction MindSwapToActionOnControlledActor
    | CompletePlayerAction StartSession
    | CompletePlayerAction StartSessionWithUnknownTileset
    | CompletePlayerAction ToggleTileSetAction
    | CompleteAnyoneAction WaitAction
    | CompletePlayerAction HelpAction
    | CompletePlayerAction QuitAction
    | CompletePlayerAction CancelAction
    | CompletePlayerAction SaveGameAction
    | CompletePlayerAction UnknownAction
    | IncompleteAction OpenAction
    | IncompleteAction CloseAction
    | IncompleteAction MindSwapAction -> combatState
