namespace Frogue
module Script =
    open Types
    let decideAction gameState =
        let actor = gameState.Actors.Head
        match actor.Script with
        | WaitScript -> CompleteAction WaitAction
