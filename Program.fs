namespace Frogue
module Main =
    open System
    open Types

    let levelMap = [
        "#######";
        "#  +  #";
        "#  #  #";
        "#######"
    ];

    let printMap map =
        Console.Clear()
        for row in map do
            Console.Write(row + "\n")

    let startingPosition = {X = 1; Y = 1}
    let statusBar = {
        Start = {X = 0; Y = 6};
        Length = 35
        }

    let getCoord(x, y) =
        match (x, y) with
        | (x, y) when
            x < 0
            || x >= List.length(levelMap)
            || y < 0
            || y >= levelMap.[1].Length
            -> failwith "position out of map bounds"
        | (x, y) -> Char.ToString(levelMap.[y].[x])

    let writeAt pos symb  =
        Console.SetCursorPosition(pos.X, pos.Y)
        Console.Write(symb: char)

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

    let writeBox(str, box, reset) =
        if String.length(str) > box.Length then failwith "String larger than box length"
        Console.SetCursorPosition(box.Start.X, box.Start.Y)
        String.replicate box.Length " "
        |> Console.Write
        Console.SetCursorPosition(box.Start.X, box.Start.Y)
        Console.Write(string str)
        if reset then Console.SetCursorPosition(0, 0)
        0

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
        let player = {Position = startingPosition}
        writeAt startingPosition '@'
        writeBox("Ready.", statusBar, true)
        |> ignore
        mainLoop 0
        let coord = getCoord(3, 1)
        writeBox("Symbol at (3, 1): " + coord, statusBar, false)
        |> ignore
        0 // return an integer exit code
