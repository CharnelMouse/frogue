namespace Frogue
module Action =
    open Types
    open Frogue.Map
    open Output

    let writeStatusAndPass gameState str reset =
        writeBox str gameState.StatusBar reset
        gameState

    let moveAction gameState newPos =
        let newGameState = {
            Player = {
                Position = newPos
            }
            Map = gameState.Map
            StatusBar = gameState.StatusBar
        }
        drawTileAt gameState.Player.Position gameState.Map
        writeAt newPos '@'
        newGameState

    let mutateSingleChar str index char =
        String.mapi (fun i x -> if i = index then char else x) str

    let openDoorAction gameState pos =
        let map = gameState.Map
        let newTiles = List.mapi (fun i x -> if i = pos.Y then mutateSingleChar x pos.X '-' else x) map.Tiles
        let newMap = createMap map.Width map.Height newTiles
        let newGameState = {Player = gameState.Player; Map = newMap; StatusBar = gameState.StatusBar}
        drawTileAt pos newMap
        writeStatusAndPass newGameState "You open the door." true

    let resolveMoveCommand gameState direction =
        let {Player = {Position = oldPos}; Map = map; StatusBar = _} = gameState
        let {X = oldX; Y = oldY} = oldPos
        let newPos =
            match direction with
            | North -> {X = oldX; Y = oldY - 1}
            | South -> {X = oldX; Y = oldY + 1}
            | West -> {X = oldX - 1; Y = oldY}
            | East -> {X = oldX + 1; Y = oldY}
        if not (posIsOnMap newPos gameState.Map)
            then writeStatusAndPass gameState "There's nothing here!" true
        else
            let targetTileType = posTileType newPos gameState.Map
            match targetTileType with
            | Wall -> writeStatusAndPass gameState "You bump up against the wall." true
            | ClosedDoor -> openDoorAction gameState newPos
            | _ -> moveAction gameState newPos

    let resolveCommand gameState command =
        match command with
        | Quit -> writeStatusAndPass gameState "Bye." false // assumes status bar is last line
        | Help -> writeStatusAndPass gameState "Move: arrow keys Wait: . Quit: q" true
        | Wait -> writeStatusAndPass gameState "Waiting..." true
        | Move direction -> resolveMoveCommand gameState direction
        | UnknownCommand -> writeStatusAndPass gameState "Unknown command, type ? for help." true
