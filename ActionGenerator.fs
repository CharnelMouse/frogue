module ActionGenerator
open Types
open Input
open CommandParser
open Script

let generateAction worldState action =
    let currentActorID = worldState.ActorCombatQueue.Head
    let currentActor =
        worldState.Actors
        |> Map.find currentActorID
    match currentActor.Controller with
    | Player ->
        getCommand action
        |> resolveCommand worldState
    | AIController ->
        CompleteAnyoneAction (decideAction worldState)
