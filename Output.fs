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

    let private pushStatus text gameState =
        let newStream =
            match gameState.StatusBuffer.Stream with
            | "" -> text
            | a -> a + " " + text
        {gameState with
            StatusBuffer = {gameState.StatusBuffer with Stream = newStream}
        }

    let private statusByController selfStatus otherSuffix currentActor receiver =
        let actorName = currentActor.Name
        match currentActor.Controller with
        | a when a = receiver -> selfStatus
        | _ -> "The " + actorName + " " + otherSuffix

    let private pushStatusByController selfStatus otherSuffix gameState =
        let text = statusByController selfStatus otherSuffix gameState.Actors.Head gameState.StatusBuffer.Receiver
        pushStatus text gameState

    let popStatus reset gameState =
        writeBox gameState.StatusBuffer.Stream gameState.StatusBar reset
        {gameState with StatusBuffer = {gameState.StatusBuffer with Stream = ""}}

    let popStatusIfReceiverTurn reset gameState =
        if gameState.StatusBuffer.Receiver = gameState.Actors.Head.Controller
            then popStatus reset gameState
            else gameState

    let updateOutput gameState =
        match gameState.Action with
        | CompletePlayerAction StartSession ->
            printMap gameState.Tileset gameState.Map
            printActors gameState.Tileset gameState.Actors
            pushStatus "Ready." gameState
        | CompletePlayerAction StartSessionWithUnknownTileset ->
            printMap gameState.Tileset gameState.Map
            printActors gameState.Tileset gameState.Actors
            pushStatus "Save game contained unknown tileset, switching to default." gameState
        | CompleteAnyoneAction (MoveAction (origin, destination)) ->
            drawTileAt origin gameState.Map gameState.Tileset
            writeAt destination (getOutputActorTile gameState.Tileset gameState.Actors.Head.Tile)
            gameState
        | BlockedAction MoveActionBlockedByAlly -> pushStatus "There's an ally there!" gameState
        | BlockedAction MoveActionBlockedByVoid -> pushStatus "There's nothing there!" gameState
        | BlockedAction MoveActionBlockedByWall -> pushStatus "You bump up against the wall." gameState
        | CompleteAnyoneAction (AttackAction _) -> pushStatusByController "You miss!" "misses!" gameState
        | CompleteAnyoneAction (OpenDoorAction pos) ->
            drawTileAt pos gameState.Map gameState.Tileset
            pushStatusByController "You open the door." "opens the door." gameState
        | BlockedAction OpenToActionBlockedByVoid -> pushStatus "There's nothing there!" gameState
        | BlockedAction OpenToActionBlockedByInvalidTile -> pushStatus "There's nothing there to open!" gameState
        | IncompleteAction OpenAction -> pushStatus "Open in which direction?" gameState
        | CompleteAnyoneAction (CloseDoorAction pos) ->
            drawTileAt pos gameState.Map gameState.Tileset
            pushStatusByController "You close the door." "closes the door." gameState
        | BlockedAction CloseToActionBlockedByVoid -> pushStatus "There's nothing there!" gameState
        | BlockedAction CloseToActionBlockedByInvalidTile -> pushStatus "There's nothing there to close!" gameState
        | IncompleteAction CloseAction -> pushStatus "Close in which direction?" gameState
        | CompleteAnyoneAction WaitAction -> pushStatusByController "Waiting..." "waits." gameState
        | CompletePlayerAction HelpAction -> pushStatus "Move: arrow keys Open: o Close: c Wait: . Quit: q" gameState
        | CompletePlayerAction QuitAction -> pushStatus "Bye." gameState // assumes status bar is last line
        | CompletePlayerAction CancelAction -> pushStatus "OK." gameState
        | CompletePlayerAction SaveGameAction ->
            saveGame "save.sav" gameState
            pushStatus "Game saved." gameState
        | CompletePlayerAction ToggleTileSetAction ->
            printMap gameState.Tileset gameState.Map
            printActors gameState.Tileset gameState.Actors
            pushStatus (
                "Tileset changed to " +
                match gameState.Tileset with
                | DefaultTileset -> "default"
                | DottedTileset -> "dots"
                + "."
                ) gameState
        | CompletePlayerAction UnknownAction -> pushStatus "Unknown command, type ? for help." gameState
