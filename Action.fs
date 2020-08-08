namespace Frogue
module Action =
    open System
    open Types
    open Screen

    let moveAction gameState direction =
        let {Player = {Position = oldPos}; Map = map} = gameState
        let {X = oldX; Y = oldY} = oldPos
        let newPos =
            match direction with
            | Up -> {X = oldX; Y = oldY - 1}
            | Down -> {X = oldX; Y = oldY + 1}
            | Left -> {X = oldX - 1; Y = oldY}
            | Right -> {X = oldX + 1; Y = oldY}
        let newGameState = {
            Player = {
                Position = newPos
            }
            Map = map
        }
        drawTileAt oldPos map
        writeAt newPos '@'
        newGameState

    let writeAndPass x str box reset =
        writeBox str box reset
        x
