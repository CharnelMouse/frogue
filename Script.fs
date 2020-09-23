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
