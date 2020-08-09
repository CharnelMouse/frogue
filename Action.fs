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
            LastAction = OpenDoorAction pos}
        newGameState

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
                LastAction = MoveActionBlockedByVoid
            }
        else
            let targetTileType = posTileType newPos gameState.Map
            match targetTileType with
            | Wall -> {
                Player = {Position = oldPos}
                Map = map
                StatusBar = statusBar
                LastAction = MoveActionBlockedByWall
                }
            | ClosedDoor -> openDoorAction gameState newPos
            | _ -> {
                Player = {Position = newPos}
                Map = gameState.Map
                StatusBar = gameState.StatusBar
                LastAction = MoveAction (gameState.Player.Position, newPos)
                }

    let resolveCommand gameState command =
        let {Player = player; Map = map; StatusBar = statusBar; LastAction = _} = gameState
        match command with
        | Move direction -> resolveMoveCommand gameState direction
        | Wait -> {Player = player; Map = map; StatusBar = statusBar; LastAction = WaitAction}
        | Help -> {Player = player; Map = map; StatusBar = statusBar; LastAction = HelpAction}
        | Quit -> {Player = player; Map = map; StatusBar = statusBar; LastAction = QuitAction}
        | UnknownCommand -> {Player = player; Map = map; StatusBar = statusBar; LastAction = UnknownAction}
