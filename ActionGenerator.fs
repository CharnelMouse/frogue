namespace Frogue
module ActionGenerator =
    open Types
    open Input
    open CommandParser
    open Script
    let generateAction gameState =
        match gameState.Actors.Head.Controller with
        | Player ->
            getCommand gameState.Action
            |> resolveCommand gameState
        | AIController -> {gameState with Action = CompleteAnyoneAction (decideAction gameState)}
