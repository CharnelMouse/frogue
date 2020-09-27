namespace Frogue
module Action =
    open Types

    let private replaceSingleElementFn index replacer list =
        List.mapi (fun i x -> if i = index then replacer x else x) list

    let private changeMapTile map pos tile =
        replaceSingleElementFn pos.Y (replaceSingleElementFn pos.X (fun x -> tile)) map.Tiles
        |> Map.create map.Width map.Height

    let private changePlayerPosition gameState pos = {
        gameState with Actors = {gameState.Actors.Head with Position = pos} :: gameState.Actors.Tail
    }

    let private changeTileset gameState = 
        let newTileset = 
            match gameState.Tileset with
            | DefaultTileset -> DottedTileset
            | DottedTileset -> DefaultTileset
        {gameState with Tileset = newTileset}

    let private executeOpenDoorAction gameState pos =
        {gameState with Map = changeMapTile gameState.Map pos OpenDoorTile}

    let private executeCloseDoorAction gameState pos =
        {gameState with Map = changeMapTile gameState.Map pos ClosedDoorTile}

    let executeAction gameState =
        match gameState.Action with
        | CompleteAnyoneAction (OpenDoorAction toPos) -> executeOpenDoorAction gameState toPos
        | CompleteAnyoneAction (CloseDoorAction toPos) -> executeCloseDoorAction gameState toPos
        | CompleteAnyoneAction (MoveAction (_, newPos)) -> changePlayerPosition gameState newPos
        | CompleteAnyoneAction (AttackAction _) -> gameState
        | CompletePlayerAction ToggleTileSetAction -> changeTileset gameState
        | BlockedAction MoveActionBlockedByAlly
        | BlockedAction MoveActionBlockedByVoid
        | BlockedAction MoveActionBlockedByWall
        | BlockedAction OpenToActionBlockedByVoid
        | BlockedAction OpenToActionBlockedByInvalidTile
        | BlockedAction CloseToActionBlockedByVoid
        | BlockedAction CloseToActionBlockedByInvalidTile
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
