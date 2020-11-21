namespace Frogue
module Dijkstra =
    open Types
    open Frogue.Map

    let rec private fillAcc finalised queue legalPositions =
        if List.isEmpty queue
            then finalised
        else
            let next = List.minBy (fun (_, _, dist) -> dist) queue
            let (nextPos, nextCost, nextDist) = next
            let neighbours =
                legalPositions
                |> List.filter (fun (x, _) -> List.contains x (allNeighbours nextPos))
                |> List.filter (fun (x, _) -> List.forall (fun (y, _) -> y <> x) finalised)
            let queueWithoutNextAndNeighbours =
                queue
                |> List.filter (fun (x, _, _) -> not (List.contains x (nextPos :: (List.map (fun (pos, _) -> pos) neighbours))))
            let neighbourEntries =
                neighbours
                |> List.map (fun (pos, cost) -> (pos, cost, nextDist + nextCost))
            let minCostNeighbourAppearances =
                neighbours
                |> List.map (fun (pos, _) ->
                    List.filter (fun (y, _, _) -> y = pos) (queue @ neighbourEntries)
                    |> List.minBy (fun (_, _, y) -> y))
            legalPositions
            |> fillAcc ((nextPos, nextDist) :: finalised) (queueWithoutNextAndNeighbours @ minCostNeighbourAppearances)

    let fill starts tiles map =
        let validPositions =
            List.allPairs [0..(map.Width - 1)] [0..(map.Height - 1)]
            |> List.map (fun (x, y) -> {X = x; Y = y})
            |> List.choose (fun pos ->
                let currentTile = getTileAt pos map
                match List.tryFind (fun (tile, cost) -> currentTile = tile) tiles with
                | Some (tile, cost) -> Some (pos, cost)
                | None -> None
                )
        let (starts, nonStarts) = List.partition (fun (pos, _) -> List.contains pos starts) validPositions
        if List.isEmpty starts
            then []
        else
            let startingQueue = List.map (function (pos, cost) -> (pos, cost, 0)) starts
            fillAcc [] startingQueue nonStarts
