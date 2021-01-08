namespace Frogue
module ActionGenerator =
    open Types
    open Input
    open CommandParser
    open Script

    let generateAction worldState action =
        match worldState.Actors.Head.Controller with
        | Player ->
            getCommand action
            |> resolveCommand worldState
        | AIController ->
            CompleteAnyoneAction (decideAction worldState)
