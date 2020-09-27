namespace Frogue
module TimeSystem =
    open Types

    let private rotateActors gameState = {
        gameState with Actors = gameState.Actors.Tail @ [gameState.Actors.Head]
    }

    let updateTime gameState = 
        match gameState.Action with
            | CompleteAnyoneAction _ -> rotateActors gameState
            | _ -> gameState
