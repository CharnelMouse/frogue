namespace Frogue
module Action =
    open Types
    open Frogue.Map

    let private rotateActors gameState = {
        gameState with Actors = gameState.Actors.Tail @ [gameState.Actors.Head]
    }

    let private replaceSingleElementFn index replacer list =
        List.mapi (fun i x -> if i = index then replacer x else x) list

    let private changeMapTile map pos tile =
        replaceSingleElementFn pos.Y (replaceSingleElementFn pos.X (fun x -> tile)) map.Tiles
        |> createMap map.Width map.Height

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
        | CompleteAction (OpenDoorAction toPos) -> executeOpenDoorAction gameState toPos |> rotateActors
        | CompleteAction (CloseDoorAction toPos) -> executeCloseDoorAction gameState toPos |> rotateActors
        | CompleteAction (MoveAction (_, newPos)) -> changePlayerPosition gameState newPos |> rotateActors
        | CompleteAction ToggleTileSetAction -> changeTileset gameState
        | BlockedAction MoveActionBlockedByVoid -> gameState
        | BlockedAction MoveActionBlockedByWall -> gameState
        | BlockedAction MoveActionBlockedByActor -> gameState
        | BlockedAction OpenToActionBlockedByVoid -> gameState
        | BlockedAction OpenToActionBlockedByInvalidTile -> gameState
        | BlockedAction CloseToActionBlockedByVoid -> gameState
        | BlockedAction CloseToActionBlockedByInvalidTile -> gameState
        | CompleteAction StartSession -> gameState
        | CompleteAction StartSessionWithUnknownTileset -> gameState
        | CompleteAction WaitAction -> rotateActors gameState
        | CompleteAction HelpAction -> gameState
        | CompleteAction QuitAction -> gameState
        | CompleteAction CancelAction -> gameState
        | CompleteAction SaveGameAction -> gameState
        | CompleteAction UnknownAction -> gameState
        | IncompleteAction OpenAction -> gameState
        | IncompleteAction CloseAction -> gameState
