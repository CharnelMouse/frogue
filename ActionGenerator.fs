module ActionGenerator
open Types
open Input
open CommandParser
open ScreenWriter
open Script

let rec generateAction worldState statusState =
    let currentActorID = worldState.ActorCombatQueue.Head
    let currentActor =
        worldState.Actors
        |> Map.find currentActorID
    match currentActor.Controller with
    | Player ->
        let writeMessage msg =
            writeBox msg statusState.StatusBar true
        let action =
            getCommand writeMessage
            |> resolveCommand worldState
        match action with
        | BlockedAction ba ->
            match ba with
            | MoveActionBlockedByAlly -> writeMessage "There's an ally there!"
            | MoveActionBlockedByVoid -> writeMessage "There's nothing there!"
            | MoveActionBlockedByWall -> writeMessage "You bump up against the wall."
            | OpenToActionBlockedByVoid -> writeMessage "There's nothing there!"
            | OpenToActionBlockedByInvalidTile -> writeMessage "There's nothing there to open!"
            | CloseToActionBlockedByVoid -> writeMessage "There's nothing there!"
            | CloseToActionBlockedByInvalidTile -> writeMessage "There's nothing there to close!"
            | CloseToActionBlockedByActor -> writeMessage "There's something in the way!"
            | MindSwapToActionBlockedByVoid -> writeMessage "There's nothing there!"
            | MindSwapToActionBlockedByNoActor -> writeMessage "There's no one there!"
            | MindSwapToActionOnControlledActor -> writeMessage "You already control that!"
            generateAction worldState statusState
        | Action a ->
            a
    | AIController ->
        AnyoneAction (decideAction worldState)
