module Dijkstra
open Types
open Frogue.Map

type NodeTypeInfo = {
    Type: MapTile
    Cost: int
}

type private NodeInfo = {
    Pos: Position
    Cost: int
}

type private UnvisitedNode = {
    ItemPosition: Position
    ItemCost: int
    CurrentDistance: int
}

type VisitedNode = {
    Position: Position
    Distance: int
}

let rec private fillAcc visited unvisited nodeInfo =
    if List.isEmpty unvisited
        then visited
    else
        let next = List.minBy (fun {CurrentDistance = dist} -> dist) unvisited
        let {ItemPosition = nextPos; ItemCost = nextCost; CurrentDistance = nextDist} = next
        let neighbours =
            nodeInfo
            |> List.filter (fun {Pos = curr} -> List.contains curr (allNeighbours nextPos))
            |> List.filter (fun {Pos = curr} -> List.forall (fun fin -> fin.Position <> curr) visited)
        let unvisitedWithoutNextAndNeighbours =
            unvisited
            |> List.filter
                (fun {ItemPosition = itemPos} ->
                    not (List.contains itemPos (nextPos :: (List.map (fun {Pos = neighbourPos} -> neighbourPos) neighbours))))
        let neighbourEntries =
            neighbours
            |> List.map (fun {Pos = pos; Cost = cost} -> {ItemPosition = pos; ItemCost = cost; CurrentDistance = nextDist + nextCost})
        let minCostNeighbourAppearances =
            neighbours
            |> List.map (fun {Pos = pos} ->
                List.filter (fun {ItemPosition = y} -> y = pos) (unvisited @ neighbourEntries)
                |> List.minBy (fun {CurrentDistance = dist} -> dist))
        nodeInfo
        |> fillAcc ({Position = nextPos; Distance = nextDist} :: visited) (unvisitedWithoutNextAndNeighbours @ minCostNeighbourAppearances)

let fill destinations tiles map =
    let validPositions =
        List.allPairs [0..(map.Width - 1)] [0..(map.Height - 1)]
        |> List.map (fun (x, y) -> {X = x; Y = y})
        |> List.choose (fun pos ->
            let currentTile = getTileAt pos map
            match List.tryFind (fun t -> currentTile = t.Type) tiles with
            | Some {Cost = cost} -> Some {Pos = pos; Cost = cost}
            | None -> None
            )
    let (destinationInfo, nonDestinationInfo) =
        validPositions
        |> List.partition (fun {Pos = pos} -> List.contains pos destinations)
    if List.isEmpty destinationInfo
        then []
    else
        let startingQueue =
            destinationInfo
            |> List.map (function x -> {ItemPosition = x.Pos; ItemCost = x.Cost; CurrentDistance = 0})
        fillAcc [] startingQueue nonDestinationInfo
