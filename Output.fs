module Output
open Types
open ScreenWriter
open Status
open MapWriter

let private fakeDoorActor = {
    Name = "door"
    Tile = UnknownActorTile
    Controller = AIController
    Script = WaitScript
    }

let updateMapOutputTileset tileset action =
    match action with
    | CompletePlayerAction ToggleTileSetAction -> cycleTileset tileset
    | _ -> tileset

let private updateMapScreen combatState tileset action =
    match action with
    | CompletePlayerAction StartSession
    | CompletePlayerAction StartSessionWithUnknownTileset
    | CompletePlayerAction ToggleTileSetAction ->
        redrawMapScreen tileset combatState
    | CompleteAnyoneAction (MoveAction (origin, destination)) ->
        drawTileAt origin combatState.CombatMap tileset
        let currentActorID = combatState.ActorCombatQueue.Head
        let currentActor =
            combatState.Actors
            |> Map.find currentActorID
        writeAt destination (getOutputActorTile tileset currentActor.Tile)
    | CompleteAnyoneAction (AttackAction (_, _, position)) ->
        drawTileAt position combatState.CombatMap tileset
    | CompleteAnyoneAction (OpenDoorAction pos)
    | CompleteAnyoneAction (CloseDoorAction pos) ->
        drawTileAt pos combatState.CombatMap tileset
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
    | CompleteAnyoneAction (AttackAction (_, object, _)) ->
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

let updateOutputState tileset statusState action combatState =
    let newTileset = updateMapOutputTileset tileset action
    updateMapScreen combatState newTileset action
    let currentActorID = combatState.ActorCombatQueue.Head
    let currentActor =
        combatState.Actors
        |> Map.find currentActorID
    let newStatusState = pushActionStatus currentActor newTileset statusState action
    newTileset, newStatusState
