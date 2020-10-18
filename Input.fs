namespace Frogue
module Input =
    open System
    open Types
    open Command

    let private getNonUnicodeCommand (input: ConsoleKeyInfo) =
        match input.Key with
        | ConsoleKey.LeftArrow -> CompleteCommand (Move West)
        | ConsoleKey.RightArrow -> CompleteCommand (Move East)
        | ConsoleKey.UpArrow -> CompleteCommand (Move North)
        | ConsoleKey.DownArrow -> CompleteCommand (Move South)
        | _ -> CompleteCommand UnknownCommand

    let rec getCommand action =
        let input = Console.ReadKey(true);
        match action with
        | CompletePlayerAction _ | CompleteAnyoneAction _ | BlockedAction _ ->
            match input.KeyChar with
            | 'o' -> IncompleteCommand Open
            | 'c' -> IncompleteCommand Close
            | 'm' -> IncompleteCommand MindSwap
            | '.' -> CompleteCommand Wait
            | '?' -> CompleteCommand Help
            | 'q' -> CompleteCommand Quit
            | 's' -> CompleteCommand SaveGameCommand
            | 't' -> CompleteCommand ToggleTilesetCommand
            | '\u0000' -> input |> getNonUnicodeCommand
            | _ -> CompleteCommand UnknownCommand
        | IncompleteAction OpenAction ->
            match input.Key with
            | ConsoleKey.LeftArrow -> CompleteCommand (OpenTo West)
            | ConsoleKey.RightArrow -> CompleteCommand (OpenTo East)
            | ConsoleKey.UpArrow -> CompleteCommand (OpenTo North)
            | ConsoleKey.DownArrow -> CompleteCommand (OpenTo South)
            | ConsoleKey.Escape -> CompleteCommand (Cancel)
            | _ -> getCommand action
        | IncompleteAction CloseAction ->
            match input.Key with
            | ConsoleKey.LeftArrow -> CompleteCommand (CloseTo West)
            | ConsoleKey.RightArrow -> CompleteCommand (CloseTo East)
            | ConsoleKey.UpArrow -> CompleteCommand (CloseTo North)
            | ConsoleKey.DownArrow -> CompleteCommand (CloseTo South)
            | ConsoleKey.Escape -> CompleteCommand (Cancel)
            | _ -> getCommand action
        | IncompleteAction MindSwapAction ->
            match input.Key with
            | ConsoleKey.LeftArrow -> CompleteCommand (MindSwapTo West)
            | ConsoleKey.RightArrow -> CompleteCommand (MindSwapTo East)
            | ConsoleKey.UpArrow -> CompleteCommand (MindSwapTo North)
            | ConsoleKey.DownArrow -> CompleteCommand (MindSwapTo South)
            | ConsoleKey.Escape -> CompleteCommand (Cancel)
            | _ -> getCommand action
