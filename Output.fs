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

    let private convertInternalTilesToTiles (parser: TilesetParser) tiles =
        tiles
        |> List.map (parser >> string)
        |> List.toSeq
        |> String.concat "" 

    let private printMap map tileset =
        let tilesetParser =
            match tileset with
            | DefaultTileset -> defaultTilesetParser
            | DottedTileset -> dottedTilesetParser
        Console.Clear()
        for row in map.Tiles do
            row
            |> convertInternalTilesToTiles tilesetParser
            |> Console.WriteLine

    let private writeAt pos (symb: char)  =
        cursorTo pos
        Console.Write(symb)
        resetCursor()

    let private getOutputTile tileset x =
        match tileset with
        | DefaultTileset -> defaultTilesetParser x
        | DottedTileset -> dottedTilesetParser x

    let private drawTileAt pos map tileset =
        getTileAt pos map
        |> getOutputTile tileset
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

    let updateOutput gameState =
        match gameState.Action with
        | CompleteAction StartSession ->
            printMap gameState.Map gameState.Tileset
            writeAt gameState.Player.Position (getOutputTile gameState.Tileset PlayerTile)
            writeBox "Ready." gameState.StatusBar true
        | CompleteAction StartSessionWithUnknownTileset ->
            printMap gameState.Map gameState.Tileset
            writeAt gameState.Player.Position (getOutputTile gameState.Tileset PlayerTile)
            writeBox "Save game contained unknown tileset, switching to default." gameState.StatusBar true
        | CompleteAction (MoveAction (origin, destination)) ->
            drawTileAt origin gameState.Map gameState.Tileset
            writeAt destination (getOutputTile gameState.Tileset PlayerTile)
        | BlockedAction MoveActionBlockedByVoid -> writeBox "There's nothing there!" gameState.StatusBar true
        | BlockedAction MoveActionBlockedByWall -> writeBox "You bump up against the wall." gameState.StatusBar true
        | CompleteAction (OpenDoorAction pos) ->
            drawTileAt pos gameState.Map gameState.Tileset
            writeBox "You open the door." gameState.StatusBar true
        | BlockedAction OpenToActionBlockedByVoid -> writeBox "There's nothing there!" gameState.StatusBar true
        | BlockedAction OpenToActionBlockedByInvalidTile -> writeBox "There's nothing there to open!" gameState.StatusBar true
        | IncompleteAction OpenAction -> writeBox "Open in which direction?" gameState.StatusBar true
        | CompleteAction (CloseDoorAction pos) ->
            drawTileAt pos gameState.Map gameState.Tileset
            writeBox "You close the door." gameState.StatusBar true
        | BlockedAction CloseToActionBlockedByVoid -> writeBox "There's nothing there!" gameState.StatusBar true
        | BlockedAction CloseToActionBlockedByInvalidTile -> writeBox "There's nothing there to close!" gameState.StatusBar true
        | IncompleteAction CloseAction -> writeBox "Close in which direction?" gameState.StatusBar true
        | CompleteAction WaitAction -> writeBox "Waiting..." gameState.StatusBar true
        | CompleteAction HelpAction -> writeBox "Move: arrow keys Open: o Close: c Wait: . Quit: q" gameState.StatusBar true
        | CompleteAction QuitAction -> writeBox "Bye." gameState.StatusBar false // assumes status bar is last line
        | CompleteAction CancelAction -> writeBox "OK." gameState.StatusBar true
        | CompleteAction SaveGameAction ->
            saveGame gameState
            writeBox "Game saved." gameState.StatusBar true
        | CompleteAction ToggleTileSetAction ->
            printMap gameState.Map gameState.Tileset
            writeAt gameState.Player.Position (getOutputTile gameState.Tileset PlayerTile)
            writeBox (
                "Tileset changed to " +
                match gameState.Tileset with
                | DefaultTileset -> "default"
                | DottedTileset -> "dots"
                + "."
                ) gameState.StatusBar true
        | CompleteAction UnknownAction -> writeBox "Unknown command, type ? for help." gameState.StatusBar true
