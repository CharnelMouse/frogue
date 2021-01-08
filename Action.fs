namespace Frogue
module Action =
    open Types

    let private replaceSingleElementFn index replacer list =
        List.mapi (fun i x -> if i = index then replacer x else x) list

    let private changeMapTile map pos tile =
        replaceSingleElementFn pos.Y (replaceSingleElementFn pos.X (fun x -> tile)) map.Tiles
        |> Map.create map.Width map.Height

    let private changePlayerPosition worldState pos =
        {
            worldState with
                Actors = {worldState.Actors.Head with Position = pos} :: worldState.Actors.Tail
        }

    let private changeActorController worldState index controller =
        {
            worldState with
                Actors = List.mapi (fun i x ->
                    match i with
                    | a when a = 0 -> {x with Controller = worldState.Actors.[index].Controller}
                    | a when a = index -> {x with Controller = controller}
                    | _ -> x)
                    worldState.Actors
        }

    let private executeOpenDoorAction worldState pos =
        {worldState with
            Map = changeMapTile worldState.Map pos OpenDoorTile
        }

    let private executeCloseDoorAction worldState pos =
        {worldState with
            Map = changeMapTile worldState.Map pos ClosedDoorTile
        }

    let private removeActor worldState index =
        {worldState with
            Actors =
                worldState.Actors
                |> List.indexed
                |> List.choose (
                    fun (i, a) ->
                        match (i, a) with
                        | (j, _) when j = index -> None
                        | (_, a) -> Some a
                )
        }

    let executeAction worldState =
        match worldState.Action with
        | CompleteAnyoneAction (OpenDoorAction toPos) ->
            executeOpenDoorAction worldState toPos
        | CompleteAnyoneAction (CloseDoorAction toPos) ->
            executeCloseDoorAction worldState toPos
        | CompleteAnyoneAction (MoveAction (_, newPos)) ->
            changePlayerPosition worldState newPos
        | CompleteAnyoneAction (MindSwapActorAction (index, controller)) ->
            changeActorController worldState index controller
        | CompletePlayerAction ToggleTileSetAction -> worldState
        | CompleteAnyoneAction (AttackAction (index, _)) -> removeActor worldState index
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
        | CompleteAnyoneAction WaitAction
        | CompletePlayerAction HelpAction
        | CompletePlayerAction QuitAction
        | CompletePlayerAction CancelAction
        | CompletePlayerAction SaveGameAction
        | CompletePlayerAction UnknownAction
        | IncompleteAction OpenAction
        | IncompleteAction CloseAction
        | IncompleteAction MindSwapAction -> worldState
