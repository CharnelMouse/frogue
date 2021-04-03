module Input
open System
open Types
open Command

type private Character = char
type private Special = ConsoleKey * int
type private KeyboardInput =
| Character of Character
| Special of Special

let private keyboardInput (info: ConsoleKeyInfo) =
    match info.KeyChar, info.Key with
    | c, _ when Char.IsLetterOrDigit c || Char.IsSymbol c || Char.IsPunctuation c -> Character c
    | _, key -> Special (key, int info.Modifiers)

let private tryLookup table key =
    table
    |> Map.tryFind key

let private tryLookupWithDefault table defaultValue key =
    tryLookup table key
    |> Option.defaultValue defaultValue

let private newCommandTable = Map [
     Character 'o'                     , IncompleteCommand Open
     Character 'c'                     , IncompleteCommand Close
     Character 'm'                     , IncompleteCommand MindSwap
     Character '.'                     , Command Wait
     Character '?'                     , Command Help
     Character 'q'                     , Command Quit
     Character 's'                     , Command SaveGameCommand
     Character 't'                     , Command ToggleTilesetCommand
     Special (ConsoleKey.LeftArrow, 0) , Command (Move West)
     Special (ConsoleKey.RightArrow, 0), Command (Move East)
     Special (ConsoleKey.UpArrow, 0)   , Command (Move North)
     Special (ConsoleKey.DownArrow, 0) , Command (Move South)
]

let private getNewCommand() =
    Console.ReadKey(true)
    |> keyboardInput
    |> tryLookup newCommandTable

let private directionCommandTable = Map [
    Special (ConsoleKey.LeftArrow , 0), Some West
    Special (ConsoleKey.RightArrow, 0), Some East
    Special (ConsoleKey.UpArrow   , 0), Some North
    Special (ConsoleKey.DownArrow , 0), Some South
    Special (ConsoleKey.Escape    , 0), None
]

let rec private getDirectionOrCancel() =
    Console.ReadKey(true)
    |> keyboardInput
    |> tryLookup directionCommandTable
    |> function
    | Some maybeDirection -> maybeDirection
    | None -> getDirectionOrCancel()

let private completeDirectionalCommand command =
    match getDirectionOrCancel() with
    | Some direction -> command direction
    | None           -> Cancel

let rec getCommand messageWriter =
        let command = getNewCommand()
        match command with
        | Some (Command c) ->
            c
        | Some (IncompleteCommand Open) ->
            messageWriter "Open in which direction?"
            completeDirectionalCommand OpenTo
        | Some (IncompleteCommand Close) ->
            messageWriter "Close in which direction?"
            completeDirectionalCommand CloseTo
        | Some (IncompleteCommand MindSwap) ->
            messageWriter "Mind swap in which direction?"
            completeDirectionalCommand MindSwapTo
        | None ->
            messageWriter "Unknown command, type ? for help."
            getCommand messageWriter
