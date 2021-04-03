module Script
open Types
open Compass
open CombatMap
open CombatGraph
open ActionTypes
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
        let enemyControllerNames =
            combatState.ControllerRelations
            |> Map.filter (fun (c, _) r -> c = actor.ControllerName && r = Enemy)
            |> Map.toList
            |> List.map (fun ((_, c), _) -> c)
        let neighbourTiles = allNeighbours pos
        let neighbourIDs =
            combatState.ActorCombatPositions
            |> Map.filter (fun _ p -> List.contains p neighbourTiles)
            |> Map.toList
            |> List.map (fun (id, _) -> id)
        let neighbourControllers =
            combatState.Actors
            |> Map.filter (fun id _ -> List.contains id neighbourIDs)
        let enemyNeighbourIDs =
            neighbourControllers
            |> Map.filter (fun _ a -> List.contains a.ControllerName enemyControllerNames)
            |> Map.toList
            |> List.map (fun (id, _) -> id)
        match enemyNeighbourIDs with
        | h :: _ -> AttackAction (h, Map.find h combatState.Actors, Map.find h combatState.ActorCombatPositions)
        | [] -> WaitAction
    | DumbHunt ->
        let enemyControllerNames =
            combatState.ControllerRelations
            |> Map.filter (fun (c, _) r -> c = actor.ControllerName && r = Enemy)
            |> Map.toList
            |> List.map (fun ((_, c), _) -> c)
        let enemyIDs =
            combatState.Actors
            |> Map.filter (fun _ {ControllerName = c} -> List.contains c enemyControllerNames)
            |> Map.toList
            |> List.map (fun (id, _) -> id)
        let enemyPositions =
            combatState.ActorCombatPositions
            |> Map.filter (fun n _ -> List.contains n enemyIDs)
            |> Map.toList
            |> List.map (fun (_, p) -> p)
        let enemyMap =
            enemyPositions
            |> fillCombat combatState.CombatMap [
                {Type = EmptyTile; Cost = 1}
                {Type = OpenDoorTile; Cost = 1}
                {Type = ClosedDoorTile; Cost = 2}
                ]
        let currentNode =
            enemyMap
            |> List.tryFind (fun {NodeID = x} -> x = pos)
        match currentNode with
        | None -> WaitAction
        | Some {Distance = currentDistance} ->
            let downhillNeighbours =
                allDirections
                |> List.choose (fun dir ->
                    let posInt =
                        enemyMap
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
