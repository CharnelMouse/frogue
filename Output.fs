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
    | PlayerAction ToggleTileSetAction -> cycleTileset tileset
    | _ -> tileset

let private updateMapScreen combatState tileset action =
    match action with
    | PlayerAction StartSession
    | PlayerAction StartSessionWithUnknownTileset
    | PlayerAction ToggleTileSetAction ->
        redrawMapScreen tileset combatState
    | AnyoneAction (MoveAction (origin, destination)) ->
        drawTileAt origin combatState.CombatMap tileset
        let currentActorID = combatState.ActorCombatQueue.Head
        let currentActor =
            combatState.Actors
            |> Map.find currentActorID
        writeAt destination (getOutputActorTile tileset currentActor.Tile)
    | AnyoneAction (AttackAction (_, _, position)) ->
        drawTileAt position combatState.CombatMap tileset
    | AnyoneAction (OpenDoorAction pos)
    | AnyoneAction (CloseDoorAction pos) ->
        drawTileAt pos combatState.CombatMap tileset
    | AnyoneAction (MindSwapActorAction _)
    | AnyoneAction WaitAction
    | PlayerAction HelpAction
    | PlayerAction SaveGameAction
    | PlayerAction QuitAction
    | PlayerAction CancelAction ->
        ()

let private pushActionStatus actor tileset statusState action =
    match action with
    | PlayerAction StartSession ->
        pushStatus statusState "Ready."
    | PlayerAction StartSessionWithUnknownTileset ->
        pushStatus statusState "Save game contained unknown tileset, switching to default."
    | AnyoneAction (MoveAction _) -> statusState
    | AnyoneAction (AttackAction (_, object, _)) ->
        pushStatusByController "kill" "kills" (Some object) "!" actor statusState
    | AnyoneAction (OpenDoorAction _) ->
        pushStatusByController "open" "opens" (Some fakeDoorActor) "." actor statusState
    | AnyoneAction (CloseDoorAction _) ->
        pushStatusByController "close" "closes" (Some fakeDoorActor) "." actor statusState
    | AnyoneAction (MindSwapActorAction _) -> pushStatus statusState "Done."
    | AnyoneAction WaitAction -> pushStatusByController "wait" "waits" None "." actor statusState
    | PlayerAction HelpAction -> pushStatus statusState "Move: arrow keys Open: o Close: c Mind swap: m Wait: . Quit: q"
    | PlayerAction QuitAction -> pushStatus statusState "Bye! Press any key to exit." // assumes status bar is last line
    | PlayerAction CancelAction -> pushStatus statusState "OK."
    | PlayerAction SaveGameAction -> pushStatus statusState "Game saved."
    | PlayerAction ToggleTileSetAction ->
        "Tileset changed to " +
        match tileset with
        | DefaultTileset -> "default"
        | DottedTileset -> "dots"
        + "."
        |> pushStatus statusState

let updateOutputState tileset statusState action combatState =
    let newTileset = updateMapOutputTileset tileset action
    updateMapScreen combatState newTileset action
    let currentActorID = combatState.ActorCombatQueue.Head
    let currentActor =
        combatState.Actors
        |> Map.find currentActorID
    let newStatusState = pushActionStatus currentActor newTileset statusState action
    newTileset, newStatusState
