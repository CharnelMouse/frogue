module Script
open Types
open CombatMap
open CombatGraph
open CommandParser

let private getAnyoneAction = function
| CompleteAnyoneAction act -> Some act
| _ -> None

let decideAction worldState =
    let actorID = worldState.ActorCombatQueue.Head
    let actor =
        worldState.Actors
        |> Map.find actorID
    let {Script = script} = actor
    let pos = Map.find actorID worldState.ActorPositions
    match script with
    | WaitScript -> WaitAction
    | StandGround ->
        let neighbourTiles = allNeighbours pos
        let neighbourID =
            worldState.ActorPositions
            |> Map.tryFindKey (fun _ p -> List.contains p neighbourTiles)
        match neighbourID with
        | Some id -> AttackAction (id, worldState.Actors.[id], worldState.ActorPositions.[id])
        | None -> WaitAction
    | DumbHunt ->
        let playerIDs =
            worldState.Actors
            |> Map.filter (fun _ {Controller = c} -> c = Player)
            |> Map.toList
            |> List.map (fun (id, _) -> id)
        let playerPositions =
            worldState.ActorPositions
            |> Map.filter (fun n _ -> List.contains n playerIDs)
            |> Map.toList
            |> List.map (fun (_, p) -> p)
        let playerMap =
            playerPositions
            |> fillCombat worldState.CombatMap [
                {Type = EmptyTile; Cost = 1}
                {Type = OpenDoorTile; Cost = 1}
                {Type = ClosedDoorTile; Cost = 2}
                ]
        let currentNode =
            playerMap
            |> List.tryFind (fun {NodeID = x} -> x = pos)
        match currentNode with
        | None -> WaitAction
        | Some {Distance = currentDistance} ->
            let downhillNeighbours =
                allDirections
                |> List.choose (fun dir ->
                    let posInt =
                        playerMap
                        |> List.tryFind (fun {NodeID = y} -> neighbour pos dir = y)
                    match posInt with
                    | None -> None
                    | Some {Distance = dist} when dist >= currentDistance -> None
                    | Some {NodeID = pos; Distance = dist} -> Some (dir, pos, dist))
                |> List.sortBy (fun (_, _, cost) -> cost)
            let nonWaitDownhillActions =
                downhillNeighbours
                |> List.choose ((fun (direction, _, _) -> direction)
                                >> resolveMoveCommand worldState
                                >> getAnyoneAction)
                |> List.filter (fun act -> act <> WaitAction)
            match nonWaitDownhillActions with
            | [] -> WaitAction
            | action::_ -> action
