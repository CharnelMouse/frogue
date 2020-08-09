namespace Frogue
module Input =
    open System
    open Types

    let private getNonUnicodeCommand (input: ConsoleKeyInfo) =
        match input.Key with
        | ConsoleKey.LeftArrow -> CompleteCommand (Move West)
        | ConsoleKey.RightArrow -> CompleteCommand (Move East)
        | ConsoleKey.UpArrow -> CompleteCommand (Move North)
        | ConsoleKey.DownArrow -> CompleteCommand (Move South)
        | _ -> CompleteCommand UnknownCommand

    let rec getCommand lastAction =
        let input = Console.ReadKey(true);
        match lastAction with
        | CompleteAction _ ->
            match input.KeyChar with
            | 'o' -> IncompleteCommand Open
            | '.' -> CompleteCommand Wait
            | '?' -> CompleteCommand Help
            | 'q' -> CompleteCommand Quit
            | '\u0000' -> input |> getNonUnicodeCommand
            | _ -> CompleteCommand UnknownCommand
        | IncompleteAction OpenAction ->
            match input.Key with
            | ConsoleKey.LeftArrow -> CompleteCommand (OpenTo West)
            | ConsoleKey.RightArrow -> CompleteCommand (OpenTo East)
            | ConsoleKey.UpArrow -> CompleteCommand (OpenTo North)
            | ConsoleKey.DownArrow -> CompleteCommand (OpenTo South)
            | ConsoleKey.Escape -> CompleteCommand (Cancel)
            | _ -> getCommand lastAction
