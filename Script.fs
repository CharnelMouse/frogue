module Script
open Types
open Compass
open CombatMap
open CombatGraph
open CommandParser

let private getAnyoneAction = function
| Action (AnyoneAction act) ->
    Some act
| Action (PlayerAction _)
| BlockedAction _ ->
    None

let decideAction combatState =
    let actorID = combatState.ActorCombatQueue.Head
    let actor =
        combatState.Actors
        |> Map.find actorID
    let {Script = script} = actor
    let pos = Map.find actorID combatState.ActorCombatPositions
    match script with
    | WaitScript -> WaitAction
    | StandGround ->
        let neighbourTiles = allNeighbours pos
        let neighbourID =
            combatState.ActorCombatPositions
            |> Map.tryFindKey (fun _ p -> List.contains p neighbourTiles)
        match neighbourID with
        | Some id -> AttackAction (id, combatState.Actors.[id], combatState.ActorCombatPositions.[id])
        | None -> WaitAction
    | DumbHunt ->
        let playerIDs =
            combatState.Actors
            |> Map.filter (fun _ {Controller = c} -> c = Player)
            |> Map.toList
            |> List.map (fun (id, _) -> id)
        let playerPositions =
            combatState.ActorCombatPositions
            |> Map.filter (fun n _ -> List.contains n playerIDs)
            |> Map.toList
            |> List.map (fun (_, p) -> p)
        let playerMap =
            playerPositions
            |> fillCombat combatState.CombatMap [
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
                |> List.choose (
                    (fun (direction, _, _) -> direction)
                    >> resolveMoveCommand combatState
                    >> getAnyoneAction
                    )
                |> List.filter (fun act -> act <> WaitAction)
            match nonWaitDownhillActions with
            | [] -> WaitAction
            | action::_ -> action
