module CombatMap
open Types

let posIsOnMap pos map =
    Map.containsKey pos map.Tiles

let positionsFromDims width height =
    let xs = List.init width id
    let ys = List.init height id
    List.allPairs ys xs // iterate x faster
    |> List.map (fun (y, x) -> {X = x; Y = y})

let create width height tiles =
    if List.length tiles = width*height then
        let positions = positionsFromDims width height
        let tileMap =
            List.zip positions tiles
            |> Map.ofList
        {
            Width = width
            Height = height
            Tiles = tileMap
        }
    else
        failwith "Invalid map"

let tryGetTileAt pos map =
    Map.tryFind pos map.Tiles

let combatPositions map =
    map.Tiles
    |> Map.toList
    |> List.map (fun (pos, _) -> pos)

let neighbour {X = x; Y = y} direction =
    match direction with
    | North -> {X = x; Y = y - 1}
    | South -> {X = x; Y = y + 1}
    | East -> {X = x + 1; Y = y}
    | West -> {X = x - 1; Y = y}

let allDirections = [East; West; North; South]

let allNeighbours pos =
    pos
    |> neighbour
    |> List.map
    <| allDirections
