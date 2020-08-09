namespace Frogue
module Map =
    open Types
    type Map = {
        Tiles: string list
        Width: int
        Height: int
    }

    type GameState = {
        Player: Player
        Map: Map
        StatusBar: TextBox
        LastAction: Action
    }

    type TileType =
    | Wall
    | ClosedDoor
    | OpenDoor
    | Empty
    | UnknownTileType

    let createMap width height tiles =
        let widthIsValid = List.length(tiles) = height
        let heightIsValid = List.reduce (&&) (List.map (function x -> (String.length(x) = width)) tiles)
        if not (widthIsValid && heightIsValid)
            then failwith "Invalid map"
        {Tiles = tiles; Width = width; Height = height}

    let posIsOnMap pos map =
        let {X = x; Y = y} = pos
        x >= 0 && x < map.Width && y >= 0 && y < map.Height

    let getTileAt pos map =
        let {X = x; Y = y} = pos
        match posIsOnMap pos map with
        | false -> failwith "position out of map bounds"
        | true -> map.Tiles.[y].[x]

    let posIsTraversable pos map =
        match getTileAt pos map with
        | '#' -> false
        | _ -> true

    let posTileType pos map =
        match getTileAt pos map with
        | '#' -> Wall
        | '+' -> ClosedDoor
        | '-' -> OpenDoor
        | ' ' -> Empty
        | _ -> UnknownTileType
