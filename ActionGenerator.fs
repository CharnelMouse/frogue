namespace Frogue
module ActionGenerator =
    open Types
    open Input
    open CommandParser
    let generateAction gameState =
        match gameState.Actors.Head.Controller with
        | Player ->
            getCommand gameState.Action
            |> resolveCommand gameState
        | AI -> resolveCommand gameState (CompleteCommand Wait)
