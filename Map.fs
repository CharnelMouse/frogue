namespace Frogue
module Map =
    open Types

    let posIsOnMap pos map =
        let {X = x; Y = y} = pos
        x >= 0 && x < map.Width && y >= 0 && y < map.Height

    let create width height tiles =
        let widthIsValid = List.length(tiles) = height
        let heightIsValid = List.reduce (&&) (List.map (function x -> (List.length(x) = width)) tiles)
        if not (widthIsValid && heightIsValid)
            then failwith "Invalid map"
        {
            Width = width
            Height = height
            Tiles = tiles
        }

    let getTileAt pos map =
        let {X = x; Y = y} = pos
        match posIsOnMap pos map with
        | false -> failwith "position out of map bounds"
        | true -> map.Tiles.[y].[x]

    let neighbour {X = x; Y = y} direction =
        match direction with
        | North -> {X = x; Y = y - 1}
        | South -> {X = x; Y = y + 1}
        | East -> {X = x + 1; Y = y}
        | West -> {X = x - 1; Y = y}

    let allDirections = [East; West; North; South]

    let allNeighbours pos =
        List.map (neighbour pos) allDirections
