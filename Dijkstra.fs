module Dijkstra

type Distance = int

type NodeInfo<'T when 'T : comparison> = {
    NodeID: 'T
    Cost: Distance
}

let nodeInfo id cost = {
    NodeID = id
    Cost = cost
}

type EdgeInfo<'T when 'T : comparison> = Map<'T, ('T * Distance) list>

type private UnvisitedNode<'T when 'T : comparison> = {
    Node: NodeInfo<'T>
    CurrentDistance: Distance
}

type VisitedNode<'T when 'T : comparison> = {
    NodeID: 'T
    Distance: Distance
}

let private visitedNode id distance = {
    NodeID = id
    Distance = distance
}

let private cons x y = x :: y

let private addDist dist node =
    {Node = node; CurrentDistance = dist}

let rec private fillAcc (neighbourInfo: EdgeInfo<'T>) visited unvisited =
    match unvisited with
    | [] -> visited
    | _ ->
        let {Node = {NodeID = nextNodeID; Cost = nextCost}; CurrentDistance = nextDist} =
            List.minBy (fun {CurrentDistance = dist} -> dist) unvisited
        let nextNeighbours =
            neighbourInfo
            |> Map.find nextNodeID
            |> List.filter (fun (curr, _) -> List.forall (fun fin -> fin.NodeID <> curr) visited)
        let nextAndNeighbourIDs =
            nextNeighbours
            |> List.map (fun (id, _) -> id)
            |> cons nextNodeID
        let unvisitedWithoutNextAndNeighbours =
            unvisited
            |> List.filter
                (fun {Node = {NodeID = itemNodeID}} ->
                    nextAndNeighbourIDs
                    |> List.contains itemNodeID
                    |> not
                    )
        let neighbourEntries =
            nextNeighbours
            |> List.map (fun (id, cost) ->
                {NodeID = id; Cost = cost}
                |> addDist (nextDist + nextCost)
                )
        let minCostNeighbourAppearances =
            nextNeighbours
            |> List.map (fun (id, _) ->
                (unvisited @ neighbourEntries)
                |> List.filter (fun {Node = {NodeID = y}} -> y = id)
                |> List.minBy (fun {CurrentDistance = dist} -> dist))
        fillAcc neighbourInfo ((visitedNode nextNodeID nextDist) :: visited) (unvisitedWithoutNextAndNeighbours @ minCostNeighbourAppearances)

let fill (nodes: NodeInfo<'T> list) edges destinations = 
    nodes
    |> List.choose (fun nodeInfo ->
        match (List.contains nodeInfo.NodeID destinations) with
        | true -> Some (addDist 0 nodeInfo)
        | false -> None
        )
    |> fillAcc edges []
