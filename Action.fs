namespace Frogue
module Action =
    open Types
    open Frogue.Map

    let private mutateSingleChar str index char =
        String.mapi (fun i x -> if i = index then char else x) str

    let private openDoorAction gameState pos =
        let map = gameState.Map
        let newTiles = List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X '-' else x) map.Tiles
        let newMap = createMap map.Width map.Height newTiles
        let newGameState = {
            Player = gameState.Player
            Map = newMap
            StatusBar = gameState.StatusBar
            Action = CompleteAction (OpenDoorAction pos)
            Tileset = gameState.Tileset}
        newGameState

    let private closeDoorAction gameState pos =
        let map = gameState.Map
        let newTiles = List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X '+' else x) map.Tiles
        let newMap = createMap map.Width map.Height newTiles
        let newGameState = {
            Player = gameState.Player
            Map = newMap
            StatusBar = gameState.StatusBar
            Action = CompleteAction (CloseDoorAction pos)
            Tileset = gameState.Tileset}
        newGameState

    let private resolveOpenToCommand gameState direction =
        let {Player = {Position = pos}} = gameState
        let toPos =
            match direction with
            | North -> {X = pos.X; Y = pos.Y - 1}
            | South -> {X = pos.X; Y = pos.Y + 1}
            | West -> {X = pos.X - 1; Y = pos.Y}
            | East -> {X = pos.X + 1; Y = pos.Y}
        if not (posIsOnMap toPos gameState.Map)
            then {
                Player = {Position = pos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                Action = BlockedAction OpenToActionBlockedByVoid
                Tileset = gameState.Tileset
            }
        else
            let targetTileType = getTileAt toPos gameState.Map
            match targetTileType with
            | ClosedDoorTile -> openDoorAction gameState toPos
            | _ -> {
                Player = {Position = pos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                Action = BlockedAction OpenToActionBlockedByInvalidTile
                Tileset = gameState.Tileset
                }

    let private resolveCloseToCommand gameState direction =
        let {Player = {Position = pos}} = gameState
        let toPos =
            match direction with
            | North -> {X = pos.X; Y = pos.Y - 1}
            | South -> {X = pos.X; Y = pos.Y + 1}
            | West -> {X = pos.X - 1; Y = pos.Y}
            | East -> {X = pos.X + 1; Y = pos.Y}
        if not (posIsOnMap toPos gameState.Map)
            then {
                Player = {Position = pos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                Action = BlockedAction CloseToActionBlockedByVoid
                Tileset = gameState.Tileset
            }
        else
            let targetTileType = getTileAt toPos gameState.Map
            match targetTileType with
            | OpenDoorTile -> closeDoorAction gameState toPos
            | _ -> {
                Player = {Position = pos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                Action = BlockedAction CloseToActionBlockedByInvalidTile
                Tileset = gameState.Tileset
                }

    let private resolveMoveCommand gameState direction =
        let {Player = {Position = oldPos}; Map = map; StatusBar = statusBar; Action = _} = gameState
        let {X = oldX; Y = oldY} = oldPos
        let newPos =
            match direction with
            | North -> {X = oldX; Y = oldY - 1}
            | South -> {X = oldX; Y = oldY + 1}
            | West -> {X = oldX - 1; Y = oldY}
            | East -> {X = oldX + 1; Y = oldY}
        if not (posIsOnMap newPos gameState.Map)
            then {
                Player = {Position = oldPos}
                Map = map
                StatusBar = statusBar
                Action = BlockedAction MoveActionBlockedByVoid
                Tileset = gameState.Tileset
            }
        else
            let targetTileType = getTileAt newPos gameState.Map
            match targetTileType with
            | WallTile -> {
                Player = {Position = oldPos}
                Map = map
                StatusBar = statusBar
                Action = BlockedAction MoveActionBlockedByWall
                Tileset = gameState.Tileset
                }
            | ClosedDoorTile -> openDoorAction gameState newPos
            | _ -> {
                Player = {Position = newPos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                Action = CompleteAction (MoveAction (gameState.Player.Position, newPos))
                Tileset = gameState.Tileset
                }

    let changeAction gameState action =
        {
            Player = gameState.Player
            Map = gameState.Map
            StatusBar = gameState.StatusBar
            Action = action
            Tileset = gameState.Tileset
        }

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
        | CompleteCommand ToggleTilesetCommand ->
            let newGameState = {
                Player = gameState.Player
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                Action = CompleteAction ToggleTileSetAction
                Tileset =
                    match gameState.Tileset with
                    | DefaultTileset -> DottedTileset
                    | DottedTileset -> DefaultTileset
            }
            newGameState
        | CompleteCommand UnknownCommand -> changeAction gameState (CompleteAction UnknownAction)
        | IncompleteCommand Open -> changeAction gameState (IncompleteAction OpenAction)
        | IncompleteCommand Close -> changeAction gameState (IncompleteAction CloseAction)
