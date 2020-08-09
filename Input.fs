namespace Frogue
module Input =
    open System
    open Types

    let private getNonUnicodeCommand(input: ConsoleKeyInfo) =
        match input.Key with
        | ConsoleKey.LeftArrow -> Move West
        | ConsoleKey.RightArrow -> Move East
        | ConsoleKey.UpArrow -> Move North
        | ConsoleKey.DownArrow -> Move South
        | _ -> UnknownCommand

    let rec getCommand() =
        let input = Console.ReadKey(true);
        match input.KeyChar with
        | '.' -> Wait
        | '?' -> Help
        | 'q' -> Quit
        | '\u0000' -> input |> getNonUnicodeCommand
        | _ -> UnknownCommand
