namespace Frogue
module Action =
    open Types
    open Frogue.Map
    open Output

    type Action =
    | StatusAction
    | MoveAction
    | OpenDoorAction

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

    let resolveMoveCommand gameState direction =
        let {Player = {Position = oldPos}; Map = map; StatusBar = _} = gameState
        let {X = oldX; Y = oldY} = oldPos
        let newPos =
            match direction with
            | Up -> {X = oldX; Y = oldY - 1}
            | Down -> {X = oldX; Y = oldY + 1}
            | Left -> {X = oldX - 1; Y = oldY}
            | Right -> {X = oldX + 1; Y = oldY}
        if not (posIsOnMap newPos gameState.Map)
            then writeStatusAndPass gameState "There's nothing here!" true
        else
            let targetTileType = posTileType newPos gameState.Map
            match targetTileType with
            | Wall -> writeStatusAndPass gameState "You bump up against the wall." true
            | _ -> moveAction gameState newPos
