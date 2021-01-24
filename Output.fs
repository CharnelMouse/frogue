module Output
open Types
open SaveSystem
open ScreenWriter
open Status
open MapWriter

let private fakeDoorActor = {
    Name = "door"
    Position = {X = 0; Y = 0}
    Tile = UnknownActorTile
    Controller = AIController
    Script = WaitScript
    }

let updateOutput worldState outputState action =
    match action with
    | CompletePlayerAction StartSession ->
        printMap outputState.Tileset worldState.Map
        printActors outputState.Tileset worldState.Actors
        pushStatus "Ready." outputState
    | CompletePlayerAction StartSessionWithUnknownTileset ->
        printMap outputState.Tileset worldState.Map
        printActors outputState.Tileset worldState.Actors
        pushStatus "Save game contained unknown tileset, switching to default." outputState
    | CompleteAnyoneAction (MoveAction (origin, destination)) ->
        drawTileAt origin worldState.Map outputState.Tileset
        writeAt destination (getOutputActorTile outputState.Tileset worldState.Actors.Head.Tile)
        outputState
    | BlockedAction MoveActionBlockedByAlly -> pushStatus "There's an ally there!" outputState
    | BlockedAction MoveActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction MoveActionBlockedByWall -> pushStatus "You bump up against the wall." outputState
    | CompleteAnyoneAction (AttackAction (_, object)) ->
        drawTileAt object.Position worldState.Map outputState.Tileset
        pushStatusByController "kill" "kills" (Some object) "!" worldState outputState
    | CompleteAnyoneAction (OpenDoorAction pos) ->
        drawTileAt pos worldState.Map outputState.Tileset
        pushStatusByController "open" "opens" (Some fakeDoorActor) "." worldState outputState
    | BlockedAction OpenToActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction OpenToActionBlockedByInvalidTile -> pushStatus "There's nothing there to open!" outputState
    | IncompleteAction OpenAction -> pushStatus "Open in which direction?" outputState
    | CompleteAnyoneAction (CloseDoorAction pos) ->
        drawTileAt pos worldState.Map outputState.Tileset
        pushStatusByController "close" "closes" (Some fakeDoorActor) "." worldState outputState
    | BlockedAction CloseToActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction CloseToActionBlockedByInvalidTile -> pushStatus "There's nothing there to close!" outputState
    | BlockedAction CloseToActionBlockedByActor -> pushStatus "There's something in the way!" outputState
    | IncompleteAction CloseAction -> pushStatus "Close in which direction?" outputState
    | BlockedAction MindSwapToActionBlockedByVoid -> pushStatus "There's nothing there!" outputState
    | BlockedAction MindSwapToActionBlockedByNoActor -> pushStatus "There's no one there!" outputState
    | BlockedAction MindSwapToActionOnControlledActor -> pushStatus "You already control that!" outputState
    | IncompleteAction MindSwapAction -> pushStatus "Mind swap in which direction?" outputState
    | CompleteAnyoneAction (MindSwapActorAction _) -> pushStatus "Done." outputState
    | CompleteAnyoneAction WaitAction -> pushStatusByController "wait" "waits" None "." worldState outputState
    | CompletePlayerAction HelpAction -> pushStatus "Move: arrow keys Open: o Close: c Mind swap: m Wait: . Quit: q" outputState
    | CompletePlayerAction QuitAction -> pushStatus "Bye! Press any key to exit." outputState // assumes status bar is last line
    | CompletePlayerAction CancelAction -> pushStatus "OK." outputState
    | CompletePlayerAction SaveGameAction ->
        saveGame "save.sav" worldState outputState
        pushStatus "Game saved." outputState
    | CompletePlayerAction ToggleTileSetAction ->
        let newOutput = changeTileset outputState
        printMap newOutput.Tileset worldState.Map
        printActors newOutput.Tileset worldState.Actors
        pushStatus (
            "Tileset changed to " +
            match newOutput.Tileset with
            | DefaultTileset -> "default"
            | DottedTileset -> "dots"
            + "."
            ) newOutput
    | CompletePlayerAction UnknownAction -> pushStatus "Unknown command, type ? for help." outputState
