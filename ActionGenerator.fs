namespace Frogue
module ActionGenerator =
    open Types
    open Input
    open CommandParser
    let generateAction gameState =
        getCommand gameState.Action
        |> resolveCommand gameState
