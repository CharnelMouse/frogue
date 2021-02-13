module OutputActor
open Types
open ScreenWriter
open Status
open MapWriter

type OutputUpdate = {
    Action: Action
    WorldState: WorldState
}

type PopStatus = {
    Reset: bool
    FullLinesOnly: bool
}

type PopStatusIfReceiverTurnOrFullLineInBuffer = {
    Reset: bool
    CurrentActor: Actor
}

type OutputMessage =
| Update of OutputUpdate
| PushDie
| PopStatus of PopStatus
| PopStatusIfReceiverTurnOrFullLineInBuffer of PopStatusIfReceiverTurnOrFullLineInBuffer
| OutputStateRequest of AsyncReplyChannel<Tileset * StatusState>
| ReplyWhenReady of AsyncReplyChannel<unit>

type OutputActor = MailboxProcessor<OutputMessage>

let private fakeDoorActor = {
    Name = "door"
    Position = {X = 0; Y = 0}
    Tile = UnknownActorTile
    Controller = AIController
    Script = WaitScript
    }

let updateMapOutputTileset tileset action =
    match action with
    | CompletePlayerAction ToggleTileSetAction -> cycleTileset tileset
    | _ -> tileset

let private updateMapScreen worldState tileset action =
    match action with
    | CompletePlayerAction StartSession
    | CompletePlayerAction StartSessionWithUnknownTileset
    | CompletePlayerAction ToggleTileSetAction ->
        redrawMapScreen tileset worldState
    | CompleteAnyoneAction (MoveAction (origin, destination)) ->
        drawTileAt origin worldState.CombatMap tileset
        writeAt destination (getOutputActorTile tileset worldState.Actors.Head.Tile)
    | CompleteAnyoneAction (AttackAction (_, object)) ->
        drawTileAt object.Position worldState.CombatMap tileset
    | CompleteAnyoneAction (OpenDoorAction pos)
    | CompleteAnyoneAction (CloseDoorAction pos) ->
        drawTileAt pos worldState.CombatMap tileset
    | CompleteAnyoneAction (MindSwapActorAction _)
    | CompleteAnyoneAction WaitAction
    | CompletePlayerAction HelpAction
    | CompletePlayerAction SaveGameAction
    | CompletePlayerAction QuitAction
    | CompletePlayerAction CancelAction
    | CompletePlayerAction UnknownAction
    | IncompleteAction _
    | BlockedAction _ ->
        ()

let private pushActionStatus actor tileset statusState action =
    match action with
    | CompletePlayerAction StartSession ->
        pushStatus statusState "Ready."
    | CompletePlayerAction StartSessionWithUnknownTileset ->
        pushStatus statusState "Save game contained unknown tileset, switching to default."
    | CompleteAnyoneAction (MoveAction _) -> statusState
    | BlockedAction MoveActionBlockedByAlly -> pushStatus statusState "There's an ally there!"
    | BlockedAction MoveActionBlockedByVoid -> pushStatus statusState "There's nothing there!"
    | BlockedAction MoveActionBlockedByWall -> pushStatus statusState "You bump up against the wall."
    | CompleteAnyoneAction (AttackAction (_, object)) ->
        pushStatusByController "kill" "kills" (Some object) "!" actor statusState
    | CompleteAnyoneAction (OpenDoorAction _) ->
        pushStatusByController "open" "opens" (Some fakeDoorActor) "." actor statusState
    | BlockedAction OpenToActionBlockedByVoid -> pushStatus statusState "There's nothing there!"
    | BlockedAction OpenToActionBlockedByInvalidTile -> pushStatus statusState "There's nothing there to open!"
    | IncompleteAction OpenAction -> pushStatus statusState "Open in which direction?"
    | CompleteAnyoneAction (CloseDoorAction _) ->
        pushStatusByController "close" "closes" (Some fakeDoorActor) "." actor statusState
    | BlockedAction CloseToActionBlockedByVoid -> pushStatus statusState "There's nothing there!"
    | BlockedAction CloseToActionBlockedByInvalidTile -> pushStatus statusState "There's nothing there to close!"
    | BlockedAction CloseToActionBlockedByActor -> pushStatus statusState "There's something in the way!"
    | IncompleteAction CloseAction -> pushStatus statusState "Close in which direction?"
    | BlockedAction MindSwapToActionBlockedByVoid -> pushStatus statusState "There's nothing there!"
    | BlockedAction MindSwapToActionBlockedByNoActor -> pushStatus statusState "There's no one there!"
    | BlockedAction MindSwapToActionOnControlledActor -> pushStatus statusState "You already control that!"
    | IncompleteAction MindSwapAction -> pushStatus statusState "Mind swap in which direction?"
    | CompleteAnyoneAction (MindSwapActorAction _) -> pushStatus statusState "Done."
    | CompleteAnyoneAction WaitAction -> pushStatusByController "wait" "waits" None "." actor statusState
    | CompletePlayerAction HelpAction -> pushStatus statusState "Move: arrow keys Open: o Close: c Mind swap: m Wait: . Quit: q"
    | CompletePlayerAction QuitAction -> pushStatus statusState "Bye! Press any key to exit." // assumes status bar is last line
    | CompletePlayerAction CancelAction -> pushStatus statusState "OK."
    | CompletePlayerAction SaveGameAction -> pushStatus statusState "Game saved."
    | CompletePlayerAction ToggleTileSetAction ->
        "Tileset changed to " +
        match tileset with
        | DefaultTileset -> "default"
        | DottedTileset -> "dots"
        + "."
        |> pushStatus statusState
    | CompletePlayerAction UnknownAction -> pushStatus statusState "Unknown command, type ? for help."

let outputAgentBody startingTileset startingStatusState (inbox: MailboxProcessor<OutputMessage>) =
    let rec loop tileset statusState = async {
        let! msg = inbox.Receive()
        match msg with
        | Update {Action = action; WorldState = worldState} ->
            let newTileset = updateMapOutputTileset tileset action
            updateMapScreen worldState newTileset action
            let newStatusState = pushActionStatus worldState.Actors.Head newTileset statusState action
            return! loop newTileset newStatusState
        | PushDie ->
            return! loop tileset (pushDieMessage statusState)
        | PopStatus {Reset = reset; FullLinesOnly = fullLinesOnly} ->
            let newOutputState = popStatus reset fullLinesOnly statusState
            return! loop tileset newOutputState
        | PopStatusIfReceiverTurnOrFullLineInBuffer {Reset = reset; CurrentActor = currentActor} ->
            let newOutputState = popStatusIfReceiverTurnOrFullLineInBuffer reset currentActor statusState
            return! loop tileset newOutputState
        | OutputStateRequest replyChannel ->
            replyChannel.Reply(tileset, statusState)
            return! loop tileset statusState
        | ReplyWhenReady replyChannel ->
            replyChannel.Reply()
            return! loop tileset statusState
    }
    loop startingTileset startingStatusState

let startOutputAgent tileset statusState =
    outputAgentBody tileset statusState
    |> MailboxProcessor.Start
