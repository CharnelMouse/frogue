namespace Frogue
module Dijkstra =
    open Types
    open Frogue.Map

    type NodeTypeInfo = {
        Tile: MapTile
        Cost: int
    }

    type NodeInfo = {
        Pos: Position
        Cost: int
    }

    type QueueNode = {
        ItemPosition: Position
        ItemCost: int
        CurrentDistance: int
    }

    type FinalNode = {
        Position: Position
        Distance: int
    }

    let rec private fillAcc finalised queue legalPositions =
        if List.isEmpty queue
            then finalised
        else
            let next = List.minBy (fun {CurrentDistance = dist} -> dist) queue
            let {ItemPosition = nextPos; ItemCost = nextCost; CurrentDistance = nextDist} = next
            let neighbours =
                legalPositions
                |> List.filter (fun {Pos = curr} -> List.contains curr (allNeighbours nextPos))
                |> List.filter (fun {Pos = curr} -> List.forall (fun fin -> fin.Position <> curr) finalised)
            let queueWithoutNextAndNeighbours =
                queue
                |> List.filter
                    (fun {ItemPosition = itemPos} ->
                        not (List.contains itemPos (nextPos :: (List.map (fun {Pos = neighbourPos} -> neighbourPos) neighbours))))
            let neighbourEntries =
                neighbours
                |> List.map (fun {Pos = pos; Cost = cost} -> {ItemPosition = pos; ItemCost = cost; CurrentDistance = nextDist + nextCost})
            let minCostNeighbourAppearances =
                neighbours
                |> List.map (fun {Pos = pos} ->
                    List.filter (fun {ItemPosition = y} -> y = pos) (queue @ neighbourEntries)
                    |> List.minBy (fun {CurrentDistance = dist} -> dist))
            legalPositions
            |> fillAcc ({Position = nextPos; Distance = nextDist} :: finalised) (queueWithoutNextAndNeighbours @ minCostNeighbourAppearances)

    let fill starts tiles map =
        let validPositions =
            List.allPairs [0..(map.Width - 1)] [0..(map.Height - 1)]
            |> List.map (fun (x, y) -> {X = x; Y = y})
            |> List.choose (fun pos ->
                let currentTile = getTileAt pos map
                match List.tryFind (fun t -> currentTile = t.Tile) tiles with
                | Some {Cost = cost} -> Some {Pos = pos; Cost = cost}
                | None -> None
                )
        let (startPosCosts, nonStartPosCosts) =
            validPositions
            |> List.partition (fun {Pos = pos} -> List.contains pos starts)
        if List.isEmpty startPosCosts
            then []
        else
            let startingQueue =
                startPosCosts
                |> List.map (function x -> {ItemPosition = x.Pos; ItemCost = x.Cost; CurrentDistance = 0})
            fillAcc [] startingQueue nonStartPosCosts
