namespace Frogue
module Main =
    open System
    open Types
    open Screen

    let levelMap = {
        Tiles = [
            "#######";
            "#  +  #";
            "#  #  #";
            "#######"
        ]
    }

    let statusBar = {
        Start = {X = 0; Y = 6};
        Length = 35
    }

    let startingGameState = {
        Player = {Position = {X = 1; Y = 1}};
        Map = levelMap;
    }
 
    let getCoord(x, y) =
        match (x, y) with
        | (x, y) when
            x < 0
            || x >= List.length(levelMap.Tiles)
            || y < 0
            || y >= levelMap.Tiles.[1].Length
            -> failwith "position out of map bounds"
        | (x, y) -> Char.ToString(levelMap.Tiles.[y].[x])

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

    let rec mainLoop gameState =
        let command = getCommand()
        match command with
        | Quit -> ()
        | Help -> writeBox "Move: arrow keys Wait: . Quit: q" statusBar true
        | Wait -> writeBox "Waiting..." statusBar true
        | Left -> writeBox "I wanna move left!" statusBar true
        | Right -> writeBox "I wanna move right!" statusBar true
        | Up -> writeBox "I wanna move up!" statusBar true
        | Down -> writeBox "I wanna move down!" statusBar true
        | Unknown -> writeBox "Unknown command, type ? for help." statusBar true
        if command <> Quit
            then mainLoop gameState

    [<EntryPoint>]
    let main argv =
        printMap levelMap
        writeAt startingGameState.Player.Position '@'
        writeBox "Ready." statusBar true
        mainLoop startingGameState
        writeBox ("Symbol at (3, 1): " + getCoord(3, 1)) statusBar false
        0 // return an integer exit code
