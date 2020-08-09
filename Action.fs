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
                LastAction = CompleteAction OpenToActionBlockedByVoid
            }
        else
            let targetTileType = posTileType toPos gameState.Map
            match targetTileType with
            | ClosedDoor -> openDoorAction gameState toPos
            | _ -> {
                Player = {Position = pos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                LastAction = CompleteAction OpenToActionBlockedByInvalidTile
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
                LastAction = CompleteAction MoveActionBlockedByVoid
            }
        else
            let targetTileType = posTileType newPos gameState.Map
            match targetTileType with
            | Wall -> {
                Player = {Position = oldPos}
                Map = map
                StatusBar = statusBar
                LastAction = CompleteAction MoveActionBlockedByWall
                }
            | ClosedDoor -> openDoorAction gameState newPos
            | _ -> {
                Player = {Position = newPos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                LastAction = CompleteAction (MoveAction (gameState.Player.Position, newPos))
                }

    let resolveCommand gameState command =
        let {Player = player; Map = map; StatusBar = statusBar; LastAction = _} = gameState
        match command with
        | CompleteCommand (Move direction) -> resolveMoveCommand gameState direction
        | CompleteCommand (OpenTo direction) -> resolveOpenToCommand gameState direction
        | CompleteCommand Wait -> {Player = player; Map = map; StatusBar = statusBar; LastAction = CompleteAction WaitAction}
        | CompleteCommand Help -> {Player = player; Map = map; StatusBar = statusBar; LastAction = CompleteAction HelpAction}
        | CompleteCommand Quit -> {Player = player; Map = map; StatusBar = statusBar; LastAction = CompleteAction QuitAction}
        | CompleteCommand Cancel -> {Player = player; Map = map; StatusBar = statusBar; LastAction = CompleteAction CancelAction}
        | CompleteCommand UnknownCommand -> {Player = player; Map = map; StatusBar = statusBar; LastAction = CompleteAction UnknownAction}
        | IncompleteCommand Open -> {Player = player; Map = map; StatusBar = statusBar; LastAction = IncompleteAction OpenAction}
