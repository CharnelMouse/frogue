namespace Frogue
module Script =
    open Types
    let decideAction gameState =
        let actor = gameState.Actors.Head
        match actor.Script with
        | WaitScript -> CompleteAction WaitAction
        | StandGround ->
            let pos = actor.Position
            let neighbourTiles = [
                {X = pos.X; Y = pos.Y - 1}
                {X = pos.X; Y = pos.Y + 1}
                {X = pos.X - 1; Y = pos.Y}
                {X = pos.X + 1; Y = pos.Y}
            ]
            let neighbour = List.tryFind (fun x -> List.contains x.Position neighbourTiles) gameState.Actors
            match neighbour with
            | Some a -> CompleteAction (AttackAction a.Position)
            | None -> CompleteAction WaitAction
        | DumbHunt ->
            let pos = actor.Position
            let nextPlayerIndex = List.tryFindIndex (fun x -> x.Controller = Player) gameState.Actors.Tail
            match nextPlayerIndex with
            | None -> CompleteAction WaitAction
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
                let isActiveAction action =
                    match action with
                    | CompleteAction (MoveAction _)
                    | CompleteAction (OpenDoorAction _)
                    | CompleteAction (CloseDoorAction _)
                    | CompleteAction (AttackAction _) -> true
                    | _ -> false
                let activeAction =
                    List.map (CommandParser.parseMoveCommand gameState) direction
                    |> List.tryFind isActiveAction
                match activeAction with
                | Some action -> action
                | None -> CompleteAction WaitAction
