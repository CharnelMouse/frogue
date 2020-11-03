namespace Frogue
module Dijkstra =
    open Types
    open DijkstraTypes
    open Frogue.Map

    let rec private fillAcc finalised queue legalPositions =
        if List.isEmpty queue
            then finalised
        else
            let minCost =
               List.map (fun x -> x.Cost) queue
               |> List.min
            let nextIndex = 
                List.findIndex (fun x -> x.Cost = minCost) queue
            let next = queue.[nextIndex]
            let neighbours =
                allNeighbours next.Position
                |> List.filter (fun x -> List.contains x legalPositions)
                |> List.filter (fun x ->
                    finalised
                    |> List.map (fun y -> y.Position)
                    |> List.contains x
                    |> not)
            let queueWithoutNextAndNeighbours =
                queue
                |> List.filter (fun x -> not (List.contains x.Position (next.Position :: neighbours)))
            let neighbourEntries =
                neighbours
                |> List.map (fun x -> {Position = x; Cost = next.Cost + 1})
            let minCostNeighbourAppearances =
                neighbours
                |> List.map (fun x ->
                    List.filter (fun y -> y.Position = x) (queue @ neighbourEntries)
                    |> List.minBy (fun y -> y.Cost))
            legalPositions
            |> fillAcc (next :: finalised) (queueWithoutNextAndNeighbours @ minCostNeighbourAppearances)

    let fill starts tiles map =
        let startsOnMap = List.fold (fun x y -> x && posIsOnMap y map) true starts
        if not startsOnMap
            then []
        else
            let positions =
                seq {
                    for x in [0..(map.Width - 1)] do
                        for y in [0..(map.Height - 1)] do
                            if (List.contains (getTileAt {X = x; Y = y} map) tiles
                                && not (List.contains {X = x; Y = y} starts))
                                then yield  {X = x; Y = y}
                }
                |> Seq.toList
            let startCosts = List.map (fun pos -> {Position = pos; Cost = 0}) starts
            fillAcc [] startCosts positions
