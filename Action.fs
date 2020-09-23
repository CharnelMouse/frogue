namespace Frogue
module Action =
    open Types
    open Frogue.Map

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
        | CompleteAction (OpenDoorAction toPos) -> executeOpenDoorAction gameState toPos
        | CompleteAction (CloseDoorAction toPos) -> executeCloseDoorAction gameState toPos
        | CompleteAction (MoveAction (_, newPos)) -> changePlayerPosition gameState newPos
        | CompleteAction (AttackAction _) -> gameState
        | CompleteAction ToggleTileSetAction -> changeTileset gameState
        | BlockedAction MoveActionBlockedByVoid
        | BlockedAction MoveActionBlockedByWall
        | BlockedAction OpenToActionBlockedByVoid
        | BlockedAction OpenToActionBlockedByInvalidTile
        | BlockedAction CloseToActionBlockedByVoid
        | BlockedAction CloseToActionBlockedByInvalidTile
        | CompleteAction StartSession
        | CompleteAction StartSessionWithUnknownTileset
        | CompleteAction WaitAction
        | CompleteAction HelpAction
        | CompleteAction QuitAction
        | CompleteAction CancelAction
        | CompleteAction SaveGameAction
        | CompleteAction UnknownAction
        | IncompleteAction OpenAction
        | IncompleteAction CloseAction -> gameState
