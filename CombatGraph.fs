module CombatGraph
open Types
open CombatMap
open Dijkstra

type NodeTypeInfo = {
    Type: MapTile
    Cost: Distance
}

let private tileCostInfo map tiles pos =
    let currentTile = getTileAt pos map
    match List.tryFind (fun t -> currentTile = t.Type) tiles with
    | Some {Cost = cost} -> Some (nodeInfo pos cost)
    | None -> None

let private nodesFromCombatMap tiles map =
    List.allPairs [0..(map.Width - 1)] [0..(map.Height - 1)]
    |> List.map (fun (x, y) -> {X = x; Y = y})
    |> List.choose (tileCostInfo map tiles)

let private edges (nodeInfo: NodeInfo<Position> list) validPositions : EdgeInfo<Position> =
    validPositions
    |> List.map (fun p ->
        let neighbours = allNeighbours p
        validPositions
        |> List.filter (fun curr -> List.contains curr neighbours)
        |> List.map (fun pos ->
            pos,
            nodeInfo
            |> List.find (fun {NodeID = pos2} -> pos = pos2)
            |> (fun {Cost = c} -> c)
            )
        |> (function lst -> p, lst)
        )
    |> Map.ofList

let private edgesFromNodes (nodes: NodeInfo<Position> list) =
    nodes
    |> List.map (fun {NodeID = p} -> p)
    |> edges nodes

let fillCombat map tiles destinations =
    let nodes = nodesFromCombatMap tiles map
    let edges = edgesFromNodes nodes
    fill nodes edges destinations
