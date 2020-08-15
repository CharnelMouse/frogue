namespace Frogue
module Action =
    open Types
    open Frogue.Map

    let private mutateSingleChar str index char =
        String.mapi (fun i x -> if i = index then char else x) str

    let private changeMapTile map pos char =
        List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X char else x) map.TextTiles

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
        let newTiles = List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X '-' else x) map.TextTiles
        let newMap = createMap map.Width map.Height newTiles
        changeMap gameState newMap

    let private executeCloseDoorAction gameState pos =
        let map = gameState.Map
        let newTiles = changeMapTile map pos '+'
        let newMap = createMap map.Width map.Height newTiles
        changeMap gameState newMap

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
