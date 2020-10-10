namespace Frogue
module Output =
    open System
    open Types
    open Frogue.Map
    open Tilesets
    open SaveSystem

    let private cursorTo pos =
        Console.SetCursorPosition(pos.X, pos.Y)

    let private resetCursor() =
        cursorTo {X = 0; Y = 0}

    let private printMap tileset map =
        let tilesetParser =
            match tileset with
            | DefaultTileset -> defaultTilesetParser
            | DottedTileset -> dottedTilesetParser
        Console.Clear()
        for row in map.Tiles do
            row
            |> convertMapTilesToString tilesetParser.MapParser
            |> Console.WriteLine

    let private writeAt pos (symb: char)  =
        cursorTo pos
        Console.Write(symb)
        resetCursor()

    let private getOutputActorTile tileset x =
        match tileset with
        | DefaultTileset -> defaultTilesetParser.ActorParser x
        | DottedTileset -> dottedTilesetParser.ActorParser x

    let private getOutputMapTile tileset x =
        match tileset with
        | DefaultTileset -> defaultTilesetParser.MapParser x
        | DottedTileset -> dottedTilesetParser.MapParser x

    let private printActors tileset actors =
        List.iter (fun x -> writeAt x.Position (getOutputActorTile tileset x.Tile)) actors

    let private drawTileAt pos map tileset =
        getTileAt pos map
        |> getOutputMapTile tileset
        |> writeAt pos

    let private clearBox box =
        cursorTo box.Start
        String.replicate box.Length " "
        |> Console.Write

    let private writeFrom startPos str =
        cursorTo startPos
        Console.Write(str: string)

    let private writeBox str box reset =
        if String.length(str) > box.Length then failwith "String larger than box length"
        clearBox box
        writeFrom box.Start str
        if reset then resetCursor()

    let private pushStatus text outputState =
        let newStream =
            match outputState.StatusBuffer.Stream with
            | "" -> text
            | a -> a + " " + text
        {outputState with
            StatusBuffer = {outputState.StatusBuffer with Stream = newStream}
        }

    let private subjectByController subject receiver =
        match subject.Controller with
        | a when a = receiver -> "You"
        | _ -> "The " + subject.Name

    let private statusByController selfStatus otherSuffix endMark currentActor object receiver =
        let subject = subjectByController currentActor receiver
        let suffix =
            match currentActor.Controller with
            | a when a = receiver -> selfStatus
            | _ -> otherSuffix
        subject + " " + suffix +
        match object with
        | None -> endMark
        | Some a when a.Controller = receiver -> " you" + endMark
        | Some a -> " the " + a.Name + endMark

    let private pushStatusByController selfStatus otherSuffix object endMark gameState =
        let text = statusByController selfStatus otherSuffix endMark gameState.WorldState.Actors.Head object gameState.OutputState.StatusBuffer.Receiver
        pushStatus text gameState.OutputState

    let popStatus reset outputState =
        writeBox outputState.StatusBuffer.Stream outputState.StatusBar reset
        {outputState with StatusBuffer = {outputState.StatusBuffer with Stream = ""}}

    let popStatusIfReceiverTurn reset gameState =
        if gameState.OutputState.StatusBuffer.Receiver = gameState.WorldState.Actors.Head.Controller
            then popStatus reset gameState.OutputState
            else gameState.OutputState

    let fakeDoorActor = {
        Name = "door"
        Position = {X = 0; Y = 0}
        Tile = UnknownActorTile
        Controller = AIController
        Script = WaitScript
        }

    let updateOutput gameState =
        match gameState.WorldState.Action with
        | CompletePlayerAction StartSession ->
            printMap gameState.OutputState.Tileset gameState.WorldState.Map
            printActors gameState.OutputState.Tileset gameState.WorldState.Actors
            pushStatus "Ready." gameState.OutputState
        | CompletePlayerAction StartSessionWithUnknownTileset ->
            printMap gameState.OutputState.Tileset gameState.WorldState.Map
            printActors gameState.OutputState.Tileset gameState.WorldState.Actors
            pushStatus "Save game contained unknown tileset, switching to default." gameState.OutputState
        | CompleteAnyoneAction (MoveAction (origin, destination)) ->
            drawTileAt origin gameState.WorldState.Map gameState.OutputState.Tileset
            writeAt destination (getOutputActorTile gameState.OutputState.Tileset gameState.WorldState.Actors.Head.Tile)
            gameState.OutputState
        | BlockedAction MoveActionBlockedByAlly -> pushStatus "There's an ally there!" gameState.OutputState
        | BlockedAction MoveActionBlockedByVoid -> pushStatus "There's nothing there!" gameState.OutputState
        | BlockedAction MoveActionBlockedByWall -> pushStatus "You bump up against the wall." gameState.OutputState
        | CompleteAnyoneAction (AttackAction ind) ->
            let object = gameState.WorldState.Actors.[ind]
            pushStatusByController "miss" "misses" (Some object) "!" gameState
        | CompleteAnyoneAction (OpenDoorAction pos) ->
            drawTileAt pos gameState.WorldState.Map gameState.OutputState.Tileset
            pushStatusByController "open" "opens" (Some fakeDoorActor) "." gameState
        | BlockedAction OpenToActionBlockedByVoid -> pushStatus "There's nothing there!" gameState.OutputState
        | BlockedAction OpenToActionBlockedByInvalidTile -> pushStatus "There's nothing there to open!" gameState.OutputState
        | IncompleteAction OpenAction -> pushStatus "Open in which direction?" gameState.OutputState
        | CompleteAnyoneAction (CloseDoorAction pos) ->
            drawTileAt pos gameState.WorldState.Map gameState.OutputState.Tileset
            pushStatusByController "close" "closes" (Some fakeDoorActor) "." gameState
        | BlockedAction CloseToActionBlockedByVoid -> pushStatus "There's nothing there!" gameState.OutputState
        | BlockedAction CloseToActionBlockedByInvalidTile -> pushStatus "There's nothing there to close!" gameState.OutputState
        | BlockedAction CloseToActionBlockedByActor -> pushStatus "There's something in the way!" gameState.OutputState
        | IncompleteAction CloseAction -> pushStatus "Close in which direction?" gameState.OutputState
        | CompleteAnyoneAction WaitAction -> pushStatusByController "wait" "waits" None "." gameState
        | CompletePlayerAction HelpAction -> pushStatus "Move: arrow keys Open: o Close: c Wait: . Quit: q" gameState.OutputState
        | CompletePlayerAction QuitAction -> pushStatus "Bye." gameState.OutputState // assumes status bar is last line
        | CompletePlayerAction CancelAction -> pushStatus "OK." gameState.OutputState
        | CompletePlayerAction SaveGameAction ->
            saveGame "save.sav" gameState
            pushStatus "Game saved." gameState.OutputState
        | CompletePlayerAction ToggleTileSetAction ->
            printMap gameState.OutputState.Tileset gameState.WorldState.Map
            printActors gameState.OutputState.Tileset gameState.WorldState.Actors
            pushStatus (
                "Tileset changed to " +
                match gameState.OutputState.Tileset with
                | DefaultTileset -> "default"
                | DottedTileset -> "dots"
                + "."
                ) gameState.OutputState
        | CompletePlayerAction UnknownAction -> pushStatus "Unknown command, type ? for help." gameState.OutputState
