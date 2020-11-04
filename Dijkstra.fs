namespace Frogue
module Dijkstra =
    open Types
    open Frogue.Map

    let rec private fillAcc finalised queue legalPositions =
        if List.isEmpty queue
            then finalised
        else
            let next = List.minBy (fun (_, x) -> x) queue
            let (nextPos, nextCost) = next
            let neighbours =
                allNeighbours nextPos
                |> List.filter (fun x -> List.contains x legalPositions)
                |> List.filter (fun x -> List.forall (fun (y, _) -> y <> x) finalised)
            let queueWithoutNextAndNeighbours =
                queue
                |> List.filter (fun (x, _) -> not (List.contains x (nextPos :: neighbours)))
            let neighbourEntries =
                neighbours
                |> List.map (fun x -> (x, nextCost + 1))
            let minCostNeighbourAppearances =
                neighbours
                |> List.map (fun x ->
                    List.filter (fun (y, _) -> y = x) (queue @ neighbourEntries)
                    |> List.minBy (fun (_, y) -> y))
            legalPositions
            |> fillAcc (next :: finalised) (queueWithoutNextAndNeighbours @ minCostNeighbourAppearances)

    let fill starts tiles map =
        let startsOnMap = List.fold (fun x y -> x && posIsOnMap y map) true starts
        if not startsOnMap
            then []
        else
            let positions =
                List.allPairs [0..(map.Width - 1)] [0..(map.Height - 1)]
                |> List.map (fun (x, y) -> {X = x; Y = y})
                |> List.filter (fun pos -> List.contains (getTileAt pos map) tiles)
                |> List.filter (fun pos -> not (List.contains pos starts))
            let startCosts = List.map (fun pos -> (pos, 0)) starts
            fillAcc [] startCosts positions
