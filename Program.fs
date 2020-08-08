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

    let rec getCommand() =
        let input = Console.ReadKey(true);
        match input.Key with
        | ConsoleKey.LeftArrow -> Left
        | ConsoleKey.RightArrow -> Right
        | ConsoleKey.UpArrow -> Up
        | ConsoleKey.DownArrow -> Down
        | ConsoleKey.OemPeriod -> Wait
        | ConsoleKey.Q -> Quit
        | _ -> Unknown

    let rec mainLoop x =
        let input = getCommand()
        match input with
        | Quit -> ()
        | Wait -> writeBox("Waiting...", statusBar, true) |> mainLoop
        | Left -> writeBox("I wanna move left!", statusBar, true) |> mainLoop
        | Right -> writeBox("I wanna move right!", statusBar, true) |> mainLoop
        | Up -> writeBox("I wanna move up!", statusBar, true) |> mainLoop
        | Down -> writeBox("I wanna move down!", statusBar, true) |> mainLoop
        | Unknown -> writeBox("Unknown command, type ? for help.", statusBar, true) |> mainLoop

    [<EntryPoint>]
    let main argv =
        printMap levelMap
        writeAt startingGameState.Player.Position '@'
        writeBox("Ready.", statusBar, true)
        |> ignore
        mainLoop 0
        let coord = getCoord(3, 1)
        writeBox("Symbol at (3, 1): " + coord, statusBar, false)
        |> ignore
        0 // return an integer exit code
