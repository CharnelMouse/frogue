namespace Frogue
module Script =
    open Types
    open Frogue.Map
    open CommandParser

    let decideAction worldState =
        let actor = worldState.Actors.Head
        match actor.Script with
        | WaitScript -> WaitAction
        | StandGround ->
            let pos = actor.Position
            let neighbourTiles = allNeighbours pos
            let neighbourIndex =
                worldState.Actors
                |> List.tryFindIndex (fun x -> List.contains x.Position neighbourTiles)
            match neighbourIndex with
            | Some ind -> AttackAction ind
            | None -> WaitAction
        | DumbHunt ->
            let pos = actor.Position
            let playerPositions =
                worldState.Actors.Tail
                |> List.filter (fun x -> x.Controller = Player)
                |> List.map (fun x -> x.Position)
            let playerMap =
                worldState.Map
                |> Dijkstra.fill playerPositions [EmptyTile; OpenDoorTile; ClosedDoorTile]
            if List.isEmpty playerMap ||
                playerMap
                |> List.forall (fun (elPos, _) -> elPos <> pos)
                then WaitAction
            else
                let currentPosCost =
                    playerMap
                    |> List.find (fun (x, _) -> x = pos)
                let (_, currentCost) = currentPosCost
                let allDirections = [East; West; North; South]
                let downhillNeighbours =
                    allDirections
                    |> List.map (fun x ->
                        (x,
                         playerMap
                         |> List.tryFind (fun (y, _) -> neighbour pos x = y)))
                    |> List.filter (fun (_, x) -> x.IsSome)
                    |> List.map (fun (x, y) ->
                        let (u, v) = y.Value
                        (x, u, v))
                    |> List.filter (fun (_, _, cost) -> cost < currentCost)
                    |> List.sortBy (fun (_, _, cost) -> cost)
                if List.isEmpty downhillNeighbours
                    then WaitAction
                else
                    let (direction, _, _) = downhillNeighbours.Head
                    let getAnyAction action =
                        match action with
                        | CompleteAnyoneAction act -> Some act
                        | _ -> None
                    let activeAction =
                        direction
                        |> parseMoveCommand worldState
                        |> getAnyAction
                    match activeAction with
                    | Some action -> action
                    | None -> WaitAction
