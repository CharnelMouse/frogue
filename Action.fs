namespace Frogue
module Action =
    open System
    open Types
    open Screen

    let getTileAt pos map =
        let {X = x; Y = y} = pos
        match (x, y) with
        | (x, y) when
            y < 0
            || y >= List.length(map.Tiles)
            || x < 0
            || x >= map.Tiles.[1].Length
            -> failwith "position out of map bounds"
        | (x, y) -> map.Tiles.[y].[x]

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
        getTileAt oldPos map
        |> writeAt oldPos
        writeAt newPos '@'
        newGameState

    let writeAndPass x str box =
        writeBox str box true
        x
