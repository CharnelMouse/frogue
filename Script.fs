namespace Frogue
module Script =
    open Types
    open Frogue.Map
    open CommandParser

    let decideAction gameState =
        let actor = gameState.Actors.Head
        match actor.Script with
        | WaitScript -> WaitAction
        | StandGround ->
            let pos = actor.Position
            let neighbourTiles = List.map (neighbour pos) [
                North
                South
                East
                West
            ]
            let neighbour = List.tryFind (fun x -> List.contains x.Position neighbourTiles) gameState.Actors
            match neighbour with
            | Some a -> AttackAction a.Position
            | None -> WaitAction
        | DumbHunt ->
            let pos = actor.Position
            let nextPlayerIndex = List.tryFindIndex (fun x -> x.Controller = Player) gameState.Actors.Tail
            match nextPlayerIndex with
            | None -> WaitAction
            | Some ind ->
                let nextPlayerPos = gameState.Actors.Tail.[ind].Position
                let direction =
                    match nextPlayerPos with
                    | {X = x; Y = y} when x < pos.X && y < pos.Y -> [West; North]
                    | {X = x; Y = y} when x < pos.X && y = pos.Y -> [West]
                    | {X = x; Y = y} when x < pos.X && y > pos.Y -> [West; South]
                    | {X = x; Y = y} when x = pos.X && y < pos.Y -> [North]
                    | {X = x; Y = y} when x = pos.X && y = pos.Y -> []
                    | {X = x; Y = y} when x = pos.X && y > pos.Y -> [South]
                    | {X = x; Y = y} when x > pos.X && y < pos.Y -> [East; North]
                    | {X = x; Y = y} when x > pos.X && y = pos.Y -> [East]
                    | {X = x; Y = y} when x > pos.X && y > pos.Y -> [East; South]
                    | _ -> failwith "AI script failure: couldn't process target's position" // impossible?
                let getAnyAction action =
                    match action with
                    | CompleteAnyoneAction act -> Some act
                    | _ -> None
                let activeAction =
                    direction
                    |> List.map (parseMoveCommand gameState >> getAnyAction)
                    |> List.tryFind (fun x -> x.IsSome)
                match activeAction with
                | Some (Some action) -> action
                | Some None -> failwith "AI script failure: returned player-only action" // impossible?
                | None -> WaitAction
