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

    let private changeTileset gameState = 
        let newTileset = 
            match gameState.OutputState.Tileset with
            | DefaultTileset -> DottedTileset
            | DottedTileset -> DefaultTileset
        {gameState with OutputState = {gameState.OutputState with Tileset = newTileset}}

    let private executeOpenDoorAction worldState pos =
        {worldState with
            Map = changeMapTile worldState.Map pos OpenDoorTile
        }

    let private executeCloseDoorAction worldState pos =
        {worldState with
            Map = changeMapTile worldState.Map pos ClosedDoorTile
        }

    let executeAction gameState =
        match gameState.WorldState.Action with
        | CompleteAnyoneAction (OpenDoorAction toPos) ->
            {gameState with WorldState = executeOpenDoorAction gameState.WorldState toPos}
        | CompleteAnyoneAction (CloseDoorAction toPos) ->
            {gameState with WorldState = executeCloseDoorAction gameState.WorldState toPos}
        | CompleteAnyoneAction (MoveAction (_, newPos)) ->
            {gameState with WorldState = changePlayerPosition gameState.WorldState newPos}
        | CompletePlayerAction ToggleTileSetAction -> changeTileset gameState
        | CompleteAnyoneAction (AttackAction _)
        | BlockedAction MoveActionBlockedByAlly
        | BlockedAction MoveActionBlockedByVoid
        | BlockedAction MoveActionBlockedByWall
        | BlockedAction OpenToActionBlockedByVoid
        | BlockedAction OpenToActionBlockedByInvalidTile
        | BlockedAction CloseToActionBlockedByVoid
        | BlockedAction CloseToActionBlockedByInvalidTile
        | BlockedAction CloseToActionBlockedByActor
        | CompletePlayerAction StartSession
        | CompletePlayerAction StartSessionWithUnknownTileset
        | CompleteAnyoneAction WaitAction
        | CompletePlayerAction HelpAction
        | CompletePlayerAction QuitAction
        | CompletePlayerAction CancelAction
        | CompletePlayerAction SaveGameAction
        | CompletePlayerAction UnknownAction
        | IncompleteAction OpenAction
        | IncompleteAction CloseAction -> gameState
