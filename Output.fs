namespace Frogue
module Output =
    open System
    open Types
    open Frogue.Map

    let private cursorTo pos =
        Console.SetCursorPosition(pos.X, pos.Y)

    let private resetCursor() =
        cursorTo {X = 0; Y = 0}

    let printMap map =
        Console.Clear()
        for row in map.Tiles do
            Console.WriteLine(row)

    let writeAt pos symb  =
        cursorTo pos
        Console.Write(symb: char)
        resetCursor()

    let drawTileAt pos map =
        getTileAt pos map
        |> writeAt pos

    let private clearBox box =
        cursorTo box.Start
        String.replicate box.Length " "
        |> Console.Write

    let private writeFrom startPos str =
        cursorTo startPos
        Console.Write(str: string)

    let writeBox str box reset =
        if String.length(str) > box.Length then failwith "String larger than box length"
        clearBox box
        writeFrom box.Start str
        if reset then resetCursor()

    let updateOutput gameState =
        match gameState.LastAction with
        | CompleteAction StartSession ->
            printMap gameState.Map
            writeAt gameState.Player.Position '@'
            writeBox "Ready." gameState.StatusBar true
        | CompleteAction (MoveAction (origin, destination)) ->
            drawTileAt origin gameState.Map
            writeAt destination '@'
        | BlockedAction MoveActionBlockedByVoid -> writeBox "There's nothing there!" gameState.StatusBar true
        | BlockedAction MoveActionBlockedByWall -> writeBox "You bump up against the wall." gameState.StatusBar true
        | CompleteAction (OpenDoorAction pos) ->
            drawTileAt pos gameState.Map
            writeBox "You open the door." gameState.StatusBar true
        | BlockedAction OpenToActionBlockedByVoid -> writeBox "There's nothing there!" gameState.StatusBar true
        | BlockedAction OpenToActionBlockedByInvalidTile -> writeBox "There's nothing there to open!" gameState.StatusBar true
        | IncompleteAction OpenAction -> writeBox "Open in which direction?" gameState.StatusBar true
        | CompleteAction (CloseDoorAction pos) ->
            drawTileAt pos gameState.Map
            writeBox "You close the door." gameState.StatusBar true
        | BlockedAction CloseToActionBlockedByVoid -> writeBox "There's nothing there!" gameState.StatusBar true
        | BlockedAction CloseToActionBlockedByInvalidTile -> writeBox "There's nothing there to close!" gameState.StatusBar true
        | IncompleteAction CloseAction -> writeBox "Close in which direction?" gameState.StatusBar true
        | CompleteAction WaitAction -> writeBox "Waiting..." gameState.StatusBar true
        | CompleteAction HelpAction -> writeBox "Move: arrow keys Open: o Close: c Wait: . Quit: q" gameState.StatusBar true
        | CompleteAction QuitAction -> writeBox "Bye." gameState.StatusBar false // assumes status bar is last line
        | CompleteAction CancelAction -> writeBox "OK." gameState.StatusBar true
        | CompleteAction UnknownAction -> writeBox "Unknown command, type ? for help." gameState.StatusBar true
