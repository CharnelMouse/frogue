module Action
open Types

let private replaceSingleElementFn index replacer list =
    List.mapi (fun i x -> if i = index then replacer x else x) list

let private changeMapTile map pos tile =
    {map with Tiles = Map.change pos (fun _ -> Some tile) map.Tiles}

let private changePlayerPosition pos combatState =
    let actorID = combatState.ActorCombatQueue.Head
    let newPositions =
        combatState.ActorCombatPositions
        |> Map.change actorID (Option.bind (fun _ -> Some pos))
    {combatState with ActorCombatPositions = newPositions}

let private changeMaybeActorController controller maybeActor =
    match maybeActor with
    | Some actor -> Some {actor with Controller = controller}
    | None -> None

let private changeActorController id controller combatState =
    let currentActorID = combatState.ActorCombatQueue.Head
    let {Controller = targetController} =
        combatState.Actors
        |> Map.find id
    let newActors =
        combatState.Actors
        |> Map.change currentActorID (changeMaybeActorController targetController)
        |> Map.change id (changeMaybeActorController controller)
    {combatState with Actors = newActors}

let private executeOpenDoorAction pos combatState =
    {combatState with
        CombatMap = changeMapTile combatState.CombatMap pos OpenDoorTile
    }

let private executeCloseDoorAction pos combatState =
    {combatState with
        CombatMap = changeMapTile combatState.CombatMap pos ClosedDoorTile
    }

let private removeActor id combatState =
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
        executeOpenDoorAction toPos combatState
    | CompleteAnyoneAction (CloseDoorAction toPos) ->
        executeCloseDoorAction toPos combatState
    | CompleteAnyoneAction (MoveAction (_, newPos)) ->
        changePlayerPosition newPos combatState
    | CompleteAnyoneAction (MindSwapActorAction (index, controller)) ->
        changeActorController index controller combatState
    | CompleteAnyoneAction (AttackAction (id, _, _)) ->
        removeActor id combatState
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
    | IncompleteAction MindSwapAction ->
        combatState
