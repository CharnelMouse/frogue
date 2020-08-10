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
            LastAction = CompleteAction (OpenDoorAction pos)}
        newGameState

    let private closeDoorAction gameState pos =
        let map = gameState.Map
        let newTiles = List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X '+' else x) map.Tiles
        let newMap = createMap map.Width map.Height newTiles
        let newGameState = {
            Player = gameState.Player
            Map = newMap
            StatusBar = gameState.StatusBar
            LastAction = CompleteAction (CloseDoorAction pos)}
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
                LastAction = BlockedAction OpenToActionBlockedByVoid
            }
        else
            let targetTileType = getTileAt toPos gameState.Map
            match targetTileType with
            | ClosedDoorTile -> openDoorAction gameState toPos
            | _ -> {
                Player = {Position = pos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                LastAction = BlockedAction OpenToActionBlockedByInvalidTile
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
                LastAction = BlockedAction CloseToActionBlockedByVoid
            }
        else
            let targetTileType = getTileAt toPos gameState.Map
            match targetTileType with
            | OpenDoorTile -> closeDoorAction gameState toPos
            | _ -> {
                Player = {Position = pos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                LastAction = BlockedAction CloseToActionBlockedByInvalidTile
                }

    let private resolveMoveCommand gameState direction =
        let {Player = {Position = oldPos}; Map = map; StatusBar = statusBar; LastAction = _} = gameState
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
                LastAction = BlockedAction MoveActionBlockedByVoid
            }
        else
            let targetTileType = getTileAt newPos gameState.Map
            match targetTileType with
            | WallTile -> {
                Player = {Position = oldPos}
                Map = map
                StatusBar = statusBar
                LastAction = BlockedAction MoveActionBlockedByWall
                }
            | ClosedDoorTile -> openDoorAction gameState newPos
            | _ -> {
                Player = {Position = newPos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                LastAction = CompleteAction (MoveAction (gameState.Player.Position, newPos))
                }

    let changeLastAction gameState action =
        {
            Player = gameState.Player
            Map = gameState.Map
            StatusBar = gameState.StatusBar
            LastAction = action
        }

    let resolveCommand gameState command =
        match command with
        | CompleteCommand (Move direction) -> resolveMoveCommand gameState direction
        | CompleteCommand (OpenTo direction) -> resolveOpenToCommand gameState direction
        | CompleteCommand (CloseTo direction) -> resolveCloseToCommand gameState direction
        | CompleteCommand Wait -> changeLastAction gameState (CompleteAction WaitAction)
        | CompleteCommand Help -> changeLastAction gameState (CompleteAction HelpAction)
        | CompleteCommand Quit -> changeLastAction gameState (CompleteAction QuitAction)
        | CompleteCommand Cancel -> changeLastAction gameState (CompleteAction CancelAction)
        | CompleteCommand SaveGameCommand -> changeLastAction gameState (CompleteAction SaveGameAction)
        | CompleteCommand UnknownCommand -> changeLastAction gameState (CompleteAction UnknownAction)
        | IncompleteCommand Open -> changeLastAction gameState (IncompleteAction OpenAction)
        | IncompleteCommand Close -> changeLastAction gameState (IncompleteAction CloseAction)
