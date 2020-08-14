namespace Frogue
module Action =
    open Types
    open Frogue.Map

    let private mutateSingleChar str index char =
        String.mapi (fun i x -> if i = index then char else x) str

    let changeMapTile map pos char =
        List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X char else x) map.Tiles

    let private changeMap gameState map = {
        Player = gameState.Player
        Map = map
        StatusBar = gameState.StatusBar
        Action = gameState.Action
        Tileset = gameState.Tileset
    }

    let private changeAction gameState action =
        {
            Player = gameState.Player
            Map = gameState.Map
            StatusBar = gameState.StatusBar
            Action = action
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
        let newTiles = List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X '-' else x) map.Tiles
        let newMap = createMap map.Width map.Height newTiles
        changeMap gameState newMap

    let private executeCloseDoorAction gameState pos =
        let map = gameState.Map
        let newTiles = changeMapTile map pos '+'
        let newMap = createMap map.Width map.Height newTiles
        changeMap gameState newMap

    let private resolveOpenToCommand gameState direction =
        let {Player = {Position = {X = x; Y = y}}} = gameState
        let toPos =
            match direction with
            | North -> {X = x; Y = y - 1}
            | South -> {X = x; Y = y + 1}
            | West -> {X = x - 1; Y = y}
            | East -> {X = x + 1; Y = y}
        let newAction =
            if not (posIsOnMap toPos gameState.Map)
                then BlockedAction OpenToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos gameState.Map
                match targetTileType with
                | ClosedDoorTile -> CompleteAction (OpenDoorAction toPos)
                | _ -> BlockedAction OpenToActionBlockedByInvalidTile
        changeAction gameState newAction

    let private resolveCloseToCommand gameState direction =
        let {Player = {Position = pos}} = gameState
        let toPos =
            match direction with
            | North -> {X = pos.X; Y = pos.Y - 1}
            | South -> {X = pos.X; Y = pos.Y + 1}
            | West -> {X = pos.X - 1; Y = pos.Y}
            | East -> {X = pos.X + 1; Y = pos.Y}
        let newAction =
            if not (posIsOnMap toPos gameState.Map)
                then BlockedAction CloseToActionBlockedByVoid
            else
                let targetTileType = getTileAt toPos gameState.Map
                match targetTileType with
                | OpenDoorTile -> CompleteAction (CloseDoorAction toPos)
                | _ -> BlockedAction CloseToActionBlockedByInvalidTile
        changeAction gameState newAction

    let private resolveMoveCommand gameState direction =
        let {Player = {Position = oldPos}; Map = map; Action = _} = gameState
        let {X = oldX; Y = oldY} = oldPos
        let newPos =
            match direction with
            | North -> {X = oldX; Y = oldY - 1}
            | South -> {X = oldX; Y = oldY + 1}
            | West -> {X = oldX - 1; Y = oldY}
            | East -> {X = oldX + 1; Y = oldY}
        let newAction =
            if not (posIsOnMap newPos map)
                then BlockedAction MoveActionBlockedByVoid
            else
                let targetTileType = getTileAt newPos map
                match targetTileType with
                | WallTile -> BlockedAction MoveActionBlockedByWall
                | ClosedDoorTile -> CompleteAction (OpenDoorAction newPos)
                | _ -> CompleteAction (MoveAction  (oldPos, newPos))
        changeAction gameState newAction

    let resolveCommand gameState command =
        match command with
        | CompleteCommand (Move direction) -> resolveMoveCommand gameState direction
        | CompleteCommand (OpenTo direction) -> resolveOpenToCommand gameState direction
        | CompleteCommand (CloseTo direction) -> resolveCloseToCommand gameState direction
        | CompleteCommand Wait -> changeAction gameState (CompleteAction WaitAction)
        | CompleteCommand Help -> changeAction gameState (CompleteAction HelpAction)
        | CompleteCommand Quit -> changeAction gameState (CompleteAction QuitAction)
        | CompleteCommand Cancel -> changeAction gameState (CompleteAction CancelAction)
        | CompleteCommand SaveGameCommand -> changeAction gameState (CompleteAction SaveGameAction)
        | CompleteCommand ToggleTilesetCommand -> changeAction gameState (CompleteAction ToggleTileSetAction)
        | CompleteCommand UnknownCommand -> changeAction gameState (CompleteAction UnknownAction)
        | IncompleteCommand Open -> changeAction gameState (IncompleteAction OpenAction)
        | IncompleteCommand Close -> changeAction gameState (IncompleteAction CloseAction)

    let executeAction gameState =
        match gameState.Action with
        | CompleteAction (OpenDoorAction toPos) -> executeOpenDoorAction gameState toPos
        | CompleteAction (CloseDoorAction toPos) -> executeCloseDoorAction gameState toPos
        | CompleteAction (MoveAction (_, newPos)) -> {
                Player = {Position = newPos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                Action = gameState.Action
                Tileset = gameState.Tileset
            }
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
