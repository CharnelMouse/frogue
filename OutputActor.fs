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
| OutputStateRequest of AsyncReplyChannel<OutputState>
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

let private pushActionStatus actor outputState action =
    match action with
    | CompletePlayerAction StartSession ->
        pushStatus "Ready." outputState
    | CompletePlayerAction StartSessionWithUnknownTileset ->
        pushStatus "Save game contained unknown tileset, switching to default." outputState
    | CompleteAnyoneAction (MoveAction _) -> outputState
    | BlockedAction MoveActionBlockedByAlly -> pushStatus "There's an ally there!" outputState
    | BlockedAction MoveActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction MoveActionBlockedByWall -> pushStatus "You bump up against the wall." outputState
    | CompleteAnyoneAction (AttackAction (_, object)) ->
        pushStatusByController "kill" "kills" (Some object) "!" actor outputState
    | CompleteAnyoneAction (OpenDoorAction _) ->
        pushStatusByController "open" "opens" (Some fakeDoorActor) "." actor outputState
    | BlockedAction OpenToActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction OpenToActionBlockedByInvalidTile -> pushStatus "There's nothing there to open!" outputState
    | IncompleteAction OpenAction -> pushStatus "Open in which direction?" outputState
    | CompleteAnyoneAction (CloseDoorAction _) ->
        pushStatusByController "close" "closes" (Some fakeDoorActor) "." actor outputState
    | BlockedAction CloseToActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction CloseToActionBlockedByInvalidTile -> pushStatus "There's nothing there to close!" outputState
    | BlockedAction CloseToActionBlockedByActor -> pushStatus "There's something in the way!" outputState
    | IncompleteAction CloseAction -> pushStatus "Close in which direction?" outputState
    | BlockedAction MindSwapToActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction MindSwapToActionBlockedByNoActor -> pushStatus "There's no one there!" outputState
    | BlockedAction MindSwapToActionOnControlledActor -> pushStatus "You already control that!" outputState
    | IncompleteAction MindSwapAction -> pushStatus "Mind swap in which direction?" outputState
    | CompleteAnyoneAction (MindSwapActorAction _) -> pushStatus "Done." outputState
    | CompleteAnyoneAction WaitAction -> pushStatusByController "wait" "waits" None "." actor outputState
    | CompletePlayerAction HelpAction -> pushStatus "Move: arrow keys Open: o Close: c Mind swap: m Wait: . Quit: q" outputState
    | CompletePlayerAction QuitAction -> pushStatus "Bye! Press any key to exit." outputState // assumes status bar is last line
    | CompletePlayerAction CancelAction -> pushStatus "OK." outputState
    | CompletePlayerAction SaveGameAction -> pushStatus "Game saved." outputState
    | CompletePlayerAction ToggleTileSetAction ->
        pushStatus (
            "Tileset changed to " +
            match outputState.Tileset with
            | DefaultTileset -> "default"
            | DottedTileset -> "dots"
            + "."
            ) outputState
    | CompletePlayerAction UnknownAction -> pushStatus "Unknown command, type ? for help." outputState

let outputAgentBody startingOutputState (inbox: MailboxProcessor<OutputMessage>) =
    let rec loop outputState = async {
        let! msg = inbox.Receive()
        match msg with
        | Update {Action = action; WorldState = worldState} ->
            let newOutputState = {outputState with Tileset = updateMapOutputTileset outputState.Tileset action}
            updateMapScreen worldState newOutputState.Tileset action
            let newerOutputState = pushActionStatus worldState.Actors.Head newOutputState action
            return! loop newerOutputState
        | PushDie ->
            return! loop (pushDieMessage outputState)
        | PopStatus {Reset = reset; FullLinesOnly = fullLinesOnly} ->
            let newOutputState = popStatus reset fullLinesOnly outputState
            return! loop newOutputState
        | PopStatusIfReceiverTurnOrFullLineInBuffer {Reset = reset; CurrentActor = currentActor} ->
            let newOutputState = popStatusIfReceiverTurnOrFullLineInBuffer reset currentActor outputState
            return! loop newOutputState
        | OutputStateRequest replyChannel ->
            replyChannel.Reply(outputState)
            return! loop outputState
        | ReplyWhenReady replyChannel ->
            replyChannel.Reply()
            return! loop outputState
    }
    loop startingOutputState

let startOutputAgent = outputAgentBody >> MailboxProcessor.Start