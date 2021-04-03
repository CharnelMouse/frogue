module Action
open Types
open ActionTypes

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
    | Some actor -> Some {actor with ControllerName = controller}
    | None -> None

let private changeActorController id controller combatState =
    let currentActorID = combatState.ActorCombatQueue.Head
    let {ControllerName = targetController} =
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
    | AnyoneAction (OpenDoorAction toPos) ->
        executeOpenDoorAction toPos combatState
    | AnyoneAction (CloseDoorAction toPos) ->
        executeCloseDoorAction toPos combatState
    | AnyoneAction (MoveAction (_, newPos)) ->
        changePlayerPosition newPos combatState
    | AnyoneAction (MindSwapActorAction (index, controller)) ->
        changeActorController index controller combatState
    | AnyoneAction (AttackAction (id, _, _)) ->
        removeActor id combatState
    | PlayerAction ToggleTileSetAction
    | AnyoneAction WaitAction
    | PlayerAction HelpAction
    | PlayerAction QuitAction
    | PlayerAction CancelAction
    | PlayerAction SaveGameAction ->
        combatState
