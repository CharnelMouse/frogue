namespace Frogue
module Input =
    open System
    open Types

    let private getNonUnicodeCommand(input: ConsoleKeyInfo) =
        match input.Key with
        | ConsoleKey.LeftArrow -> Move Left
        | ConsoleKey.RightArrow -> Move Right
        | ConsoleKey.UpArrow -> Move Up
        | ConsoleKey.DownArrow -> Move Down
        | _ -> Unknown

    let rec getCommand() =
        let input = Console.ReadKey(true);
        match input.KeyChar with
        | '.' -> Wait
        | '?' -> Help
        | 'q' -> Quit
        | '\u0000' -> input |> getNonUnicodeCommand
        | _ -> Unknown
