namespace Frogue
module Input =
    open System
    open Types

    let getNonUnicodeCommand(input: ConsoleKeyInfo) =
        match input.Key with
        | ConsoleKey.LeftArrow -> Left
        | ConsoleKey.RightArrow -> Right
        | ConsoleKey.UpArrow -> Up
        | ConsoleKey.DownArrow -> Down
        | _ -> Unknown

    let rec getCommand() =
        let input = Console.ReadKey(true);
        match input.KeyChar with
        | '.' -> Wait
        | '?' -> Help
        | 'q' -> Quit
        | '\u0000' -> input |> getNonUnicodeCommand
        | _ -> Unknown
