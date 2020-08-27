namespace Frogue
module Action =
    open Types
    open Frogue.Map

    let private mutateSingleTile tiles index tile =
        List.mapi (fun i x -> if i = index then tile else x) tiles

    let private changeMapTile map pos tile =
        List.mapi (fun i x -> if i = pos.Y then mutateSingleTile x pos.X tile else x) map.Tiles

    let private changePlayerPosition gameState pos = {
        Player = {Position = pos}
        Map = gameState.Map
        StatusBar = gameState.StatusBar
        Action = gameState.Action
        Tileset = gameState.Tileset
    }

    let private changeMap gameState map = {
        Player = gameState.Player
        Map = map
        StatusBar = gameState.StatusBar
        Action = gameState.Action
        Tileset = gameState.Tileset
    }

    let private changeTileset gameState = {
        Player = gameState.Player
        Map = gameState.Map
        StatusBar = gameState.StatusBar
        Action = gameState.Action
        Tileset =
            match gameState.Tileset with
            | DefaultTileset -> DottedTileset
            | DottedTileset -> DefaultTileset
    }

    let private executeOpenDoorAction gameState pos =
        let map = gameState.Map
        changeMapTile map pos OpenDoorTile
        |> createMap map.Width map.Height
        |> changeMap gameState

    let private executeCloseDoorAction gameState pos =
        let map = gameState.Map
        changeMapTile map pos ClosedDoorTile
        |> createMap map.Width map.Height
        |> changeMap gameState

    let executeAction gameState =
        match gameState.Action with
        | CompleteAction (OpenDoorAction toPos) -> executeOpenDoorAction gameState toPos
        | CompleteAction (CloseDoorAction toPos) -> executeCloseDoorAction gameState toPos
        | CompleteAction (MoveAction (_, newPos)) -> changePlayerPosition gameState newPos
        | CompleteAction ToggleTileSetAction -> changeTileset gameState
        | BlockedAction MoveActionBlockedByVoid -> gameState
        | BlockedAction MoveActionBlockedByWall -> gameState
        | BlockedAction OpenToActionBlockedByVoid -> gameState
        | BlockedAction OpenToActionBlockedByInvalidTile -> gameState
        | BlockedAction CloseToActionBlockedByVoid -> gameState
        | BlockedAction CloseToActionBlockedByInvalidTile -> gameState
        | CompleteAction StartSession -> gameState
        | CompleteAction StartSessionWithUnknownTileset -> gameState
        | CompleteAction WaitAction -> gameState
        | CompleteAction HelpAction -> gameState
        | CompleteAction QuitAction -> gameState
        | CompleteAction CancelAction -> gameState
        | CompleteAction SaveGameAction -> gameState
        | CompleteAction UnknownAction -> gameState
        | IncompleteAction OpenAction -> gameState
        | IncompleteAction CloseAction -> gameState
