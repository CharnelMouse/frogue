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

    let pushDieMessage outputState =
        pushStatus "You die! Press enter to exit." outputState

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

    let private pushStatusByController selfStatus otherSuffix object endMark worldState outputState =
        let text = statusByController selfStatus otherSuffix endMark worldState.Actors.Head object outputState.StatusBuffer.Receiver
        pushStatus text outputState

    let rec popStatus reset fullLinesOnly outputState =
        let buffer = outputState.StatusBuffer
        let stream = buffer.Stream
        let bar = outputState.StatusBar
        let boxLength = bar.Length
        if stream.Length <= boxLength
            then
                writeBox stream outputState.StatusBar reset
                {outputState with StatusBuffer = {buffer with Stream = ""}}
            else
                let space = (char " ")
                let continueString = "<cont.>"
                let stopIndex = boxLength - 2 - continueString.Length
                let stopString = stream.[0..stopIndex]
                let (usedString, usedLength) =
                    match stopString.[stopIndex] with
                    | a when a = space ->
                        let usedString = stopString.TrimEnd space
                        (usedString, usedString.Length)
                    | _ ->
                        let lastSpaceIndex = stopString.LastIndexOf " "
                        let usedString = stopString.[0..lastSpaceIndex].TrimEnd space
                        (usedString, usedString.Length)
                writeBox (usedString + " " + continueString) bar reset
                Console.ReadKey(true) |> ignore
                let remainingBuffer = {buffer with Stream = stream.[usedLength..].TrimStart space}
                // if == boxLength then last line only partially shows
                // after next pushing non-receiver action shown on map,
                // doesn't seem right but best I can do without
                // looking into actions future
                if fullLinesOnly && remainingBuffer.Stream.Length <= boxLength
                    then {outputState with StatusBuffer = remainingBuffer}
                    else popStatus reset fullLinesOnly {outputState with StatusBuffer = remainingBuffer}

    let popStatusIfReceiverTurnOrFullLineInBuffer reset worldState outputState =
        let buffer = outputState.StatusBuffer
        if buffer.Receiver = worldState.Actors.Head.Controller
            then popStatus reset false outputState
            else
                if buffer.Stream.Length > outputState.StatusBar.Length
                    then popStatus reset true outputState
                    else outputState

    let private changeTileset outputState = 
        let newTileset = 
            match outputState.Tileset with
            | DefaultTileset -> DottedTileset
            | DottedTileset -> DefaultTileset
        {outputState with Tileset = newTileset}

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
        | CompletePlayerAction QuitAction -> pushStatus "Bye! Press enter to exit." outputState // assumes status bar is last line
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
