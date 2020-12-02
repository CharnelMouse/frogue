namespace Frogue
module Script =
    open Types
    open Frogue.Map
    open CommandParser

    let private allDirections = [East; West; North; South]

    let private getAnyoneAction = function
    | CompleteAnyoneAction act -> Some act
    | _ -> None

    let decideAction worldState =
        let {Script = script; Position = pos} = worldState.Actors.Head
        match script with
        | WaitScript -> WaitAction
        | StandGround ->
            let neighbourTiles = allNeighbours pos
            let neighbourIndex =
                worldState.Actors
                |> List.tryFindIndex (fun x -> List.contains x.Position neighbourTiles)
            match neighbourIndex with
            | Some ind -> AttackAction ind
            | None -> WaitAction
        | DumbHunt ->
            let playerPositions =
                worldState.Actors.Tail
                |> List.choose (fun {Position = position; Controller = controller} ->
                    match controller with
                    | Player -> Some position
                    | _ -> None)
            let playerMap =
                worldState.Map
                |> Dijkstra.fill playerPositions [
                    {Type = EmptyTile; Cost = 1}
                    {Type = OpenDoorTile; Cost = 1}
                    {Type = ClosedDoorTile; Cost = 2}
                    ]
            let currentNode =
                playerMap
                |> List.tryFind (fun {Position = x} -> x = pos)
            match currentNode with
            | None -> WaitAction
            | Some {Distance = currentDistance} ->
                let downhillNeighbours =
                    allDirections
                    |> List.choose (fun dir ->
                        let posInt =
                            playerMap
                            |> List.tryFind (fun {Position = y} -> neighbour pos dir = y)
                        match posInt with
                        | None -> None
                        | Some {Distance = dist} when dist >= currentDistance -> None
                        | Some {Position = pos; Distance = dist} -> Some (dir, pos, dist))
                    |> List.sortBy (fun (_, _, cost) -> cost)
                let nonWaitDownhillActions =
                    downhillNeighbours
                    |> List.choose ((fun (direction, _, _) -> direction)
                                    >> parseMoveCommand worldState
                                    >> getAnyoneAction)
                    |> List.filter (fun act -> act <> WaitAction)
                match nonWaitDownhillActions with
                | [] -> WaitAction
                | action::_ -> action
