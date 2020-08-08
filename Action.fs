namespace Frogue
module Action =
    open Types
    open Screen

    let writeStatusAndPass gameState str reset =
        writeBox str gameState.StatusBar reset
        gameState

    let moveAction gameState direction =
        let {Player = {Position = oldPos}; Map = map; StatusBar = statusBar} = gameState
        let {X = oldX; Y = oldY} = oldPos
        let newPos =
            match direction with
            | Up -> {X = oldX; Y = oldY - 1}
            | Down -> {X = oldX; Y = oldY + 1}
            | Left -> {X = oldX - 1; Y = oldY}
            | Right -> {X = oldX + 1; Y = oldY}
        let validMove = posIsOnMap newPos gameState.Map && posIsTraversable newPos gameState.Map
        if not validMove
            then writeStatusAndPass gameState "You bump up against the wall." true
            else
                let newGameState = {
                    Player = {
                        Position = newPos
                    }
                    Map = map
                    StatusBar = statusBar
                }
                drawTileAt oldPos map
                writeAt newPos '@'
                newGameState
