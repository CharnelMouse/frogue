namespace Frogue
module Map =
    open Types
    type InternalTile =
    | EmptyTile
    | OpenDoorTile
    | ClosedDoorTile
    | WallTile
    | PlayerTile
    | UnknownTile

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
        let internalTile =
            match posIsOnMap pos map with
            | false -> failwith "position out of map bounds"
            | true -> map.Tiles.[y].[x]
        match internalTile with
        | ' ' -> EmptyTile
        | '-' -> OpenDoorTile
        | '+' -> ClosedDoorTile
        | '#' -> WallTile
        | '@' -> PlayerTile
        | _ -> UnknownTile
