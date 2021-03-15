module Action
open Types

let private replaceSingleElementFn index replacer list =
    List.mapi (fun i x -> if i = index then replacer x else x) list

let private changeMapTile map pos tile =
    {map with Tiles = Map.change pos (fun _ -> Some tile) map.Tiles}

let private changePlayerPosition worldState pos =
    let actorID = worldState.ActorCombatQueue.Head
    let newPositions =
        worldState.ActorPositions
        |> Map.change actorID (Option.bind (fun _ -> Some pos))
    {worldState with ActorPositions = newPositions}

let private changeMaybeActorController controller maybeActor =
    match maybeActor with
    | Some actor -> Some {actor with Controller = controller}
    | None -> None

let private changeActorController worldState id controller =
    let currentActorID = worldState.ActorCombatQueue.Head
    let {Controller = targetController} =
        worldState.Actors
        |> Map.find id
    let newActors =
        worldState.Actors
        |> Map.change currentActorID (changeMaybeActorController targetController)
        |> Map.change id (changeMaybeActorController controller)
    {worldState with Actors = newActors}

let private executeOpenDoorAction worldState pos =
    {worldState with
        CombatMap = changeMapTile worldState.CombatMap pos OpenDoorTile
    }

let private executeCloseDoorAction worldState pos =
    {worldState with
        CombatMap = changeMapTile worldState.CombatMap pos ClosedDoorTile
    }

let private removeActor worldState id =
    {worldState with
        Actors =
            worldState.Actors
            |> Map.remove id
        ActorCombatQueue =
            worldState.ActorCombatQueue
            |> List.filter (fun n -> n <> id)
        ActorPositions =
            worldState.ActorPositions
            |> Map.remove id
    }

let executeAction worldState action =
    match action with
    | CompleteAnyoneAction (OpenDoorAction toPos) ->
        executeOpenDoorAction worldState toPos
    | CompleteAnyoneAction (CloseDoorAction toPos) ->
        executeCloseDoorAction worldState toPos
    | CompleteAnyoneAction (MoveAction (_, newPos)) ->
        changePlayerPosition worldState newPos
    | CompleteAnyoneAction (MindSwapActorAction (index, controller)) ->
        changeActorController worldState index controller
    | CompleteAnyoneAction (AttackAction (id, _, _)) ->
        removeActor worldState id
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
    | IncompleteAction MindSwapAction -> worldState
