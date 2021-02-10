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
    | c, _ when Char.IsLetterOrDigit c || Char.IsSymbol c -> Character c
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
     Character '.'                     , CompleteCommand Wait
     Character '?'                     , CompleteCommand Help
     Character 'q'                     , CompleteCommand Quit
     Character 's'                     , CompleteCommand SaveGameCommand
     Character 't'                     , CompleteCommand ToggleTilesetCommand
     Special (ConsoleKey.LeftArrow, 0) , CompleteCommand (Move West)
     Special (ConsoleKey.RightArrow, 0), CompleteCommand (Move East)
     Special (ConsoleKey.UpArrow, 0)   , CompleteCommand (Move North)
     Special (ConsoleKey.DownArrow, 0) , CompleteCommand (Move South)
]

let private getNewCommand() =
    Console.ReadKey(true)
    |> keyboardInput
    |> tryLookupWithDefault newCommandTable (CompleteCommand UnknownCommand)

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
    | Some direction -> CompleteCommand (command direction)
    | None           -> CompleteCommand Cancel

let getCommand action =
    match action with
    | CompletePlayerAction _ | CompleteAnyoneAction _ | BlockedAction _ ->
        getNewCommand()
    | IncompleteAction OpenAction -> completeDirectionalCommand OpenTo
    | IncompleteAction CloseAction -> completeDirectionalCommand CloseTo
    | IncompleteAction MindSwapAction -> completeDirectionalCommand MindSwapTo
