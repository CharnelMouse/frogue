namespace Frogue
module ActionGenerator =
    open Types
    open Input
    open CommandParser
    open Script
    let generateAction worldState =
        match worldState.Actors.Head.Controller with
        | Player ->
            getCommand worldState.Action
            |> resolveCommand worldState
        | AIController ->
            {worldState with
                Action = CompleteAnyoneAction (decideAction worldState)
            }
