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
        drawTileAt origin worldState.Map tileset
        writeAt destination (getOutputActorTile tileset worldState.Actors.Head.Tile)
    | CompleteAnyoneAction (AttackAction (_, object)) ->
        drawTileAt object.Position worldState.Map tileset
    | CompleteAnyoneAction (OpenDoorAction pos)
    | CompleteAnyoneAction (CloseDoorAction pos) ->
        drawTileAt pos worldState.Map tileset
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
        pushStatus "Ready." statusState
    | CompletePlayerAction StartSessionWithUnknownTileset ->
        pushStatus "Save game contained unknown tileset, switching to default." statusState
    | CompleteAnyoneAction (MoveAction _) -> statusState
    | BlockedAction MoveActionBlockedByAlly -> pushStatus "There's an ally there!" statusState
    | BlockedAction MoveActionBlockedByVoid -> pushStatus "There's nothing there!" statusState
    | BlockedAction MoveActionBlockedByWall -> pushStatus "You bump up against the wall." statusState
    | CompleteAnyoneAction (AttackAction (_, object)) ->
        pushStatusByController "kill" "kills" (Some object) "!" actor statusState
    | CompleteAnyoneAction (OpenDoorAction _) ->
        pushStatusByController "open" "opens" (Some fakeDoorActor) "." actor statusState
    | BlockedAction OpenToActionBlockedByVoid -> pushStatus "There's nothing there!" statusState
    | BlockedAction OpenToActionBlockedByInvalidTile -> pushStatus "There's nothing there to open!" statusState
    | IncompleteAction OpenAction -> pushStatus "Open in which direction?" statusState
    | CompleteAnyoneAction (CloseDoorAction _) ->
        pushStatusByController "close" "closes" (Some fakeDoorActor) "." actor statusState
    | BlockedAction CloseToActionBlockedByVoid -> pushStatus "There's nothing there!" statusState
    | BlockedAction CloseToActionBlockedByInvalidTile -> pushStatus "There's nothing there to close!" statusState
    | BlockedAction CloseToActionBlockedByActor -> pushStatus "There's something in the way!" statusState
    | IncompleteAction CloseAction -> pushStatus "Close in which direction?" statusState
    | BlockedAction MindSwapToActionBlockedByVoid -> pushStatus "There's nothing there!" statusState
    | BlockedAction MindSwapToActionBlockedByNoActor -> pushStatus "There's no one there!" statusState
    | BlockedAction MindSwapToActionOnControlledActor -> pushStatus "You already control that!" statusState
    | IncompleteAction MindSwapAction -> pushStatus "Mind swap in which direction?" statusState
    | CompleteAnyoneAction (MindSwapActorAction _) -> pushStatus "Done." statusState
    | CompleteAnyoneAction WaitAction -> pushStatusByController "wait" "waits" None "." actor statusState
    | CompletePlayerAction HelpAction -> pushStatus "Move: arrow keys Open: o Close: c Mind swap: m Wait: . Quit: q" statusState
    | CompletePlayerAction QuitAction -> pushStatus "Bye! Press any key to exit." statusState // assumes status bar is last line
    | CompletePlayerAction CancelAction -> pushStatus "OK." statusState
    | CompletePlayerAction SaveGameAction -> pushStatus "Game saved." statusState
    | CompletePlayerAction ToggleTileSetAction ->
        pushStatus (
            "Tileset changed to " +
            match tileset with
            | DefaultTileset -> "default"
            | DottedTileset -> "dots"
            + "."
            ) statusState
    | CompletePlayerAction UnknownAction -> pushStatus "Unknown command, type ? for help." statusState

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
