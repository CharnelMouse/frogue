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
                |> Dijkstra.fill playerPositions [EmptyTile; OpenDoorTile; ClosedDoorTile]
            match List.tryFind (fun (x, _) -> x = pos) playerMap with
            | None -> WaitAction
            | Some (_, currentCost) ->
                let downhillNeighbours =
                    allDirections
                    |> List.choose (fun x ->
                        let posInt =
                            playerMap
                            |> List.tryFind (fun (y, _) -> neighbour pos x = y)
                        match posInt with
                        | None -> None
                        | Some (_, int) when int >= currentCost -> None
                        | Some (pos, int) -> Some (x, pos, int))
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
