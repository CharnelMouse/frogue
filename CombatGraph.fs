module CombatGraph
open Types
open CombatMap
open Dijkstra

type CombatNodeTypeInfo = {
    Type: CombatMapTile
    Cost: Distance
}

let private tileCostInfo map tiles pos =
    tryGetTileAt pos map
    |> Option.bind (fun tile ->
        match List.tryFind (fun t -> tile = t.Type) tiles with
        | Some {Cost = cost} -> Some (nodeInfo pos cost)
        | None -> None
    )

let private nodesFromCombatMap tiles map =
    combatPositions map
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
