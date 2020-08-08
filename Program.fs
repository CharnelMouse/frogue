namespace Frogue
module Main =
    open System
    open Types
    open Input
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
 
    let getCoord x y =
        match (x, y) with
        | (x, y) when
            x < 0
            || x >= List.length(levelMap.Tiles)
            || y < 0
            || y >= levelMap.Tiles.[1].Length
            -> failwith "position out of map bounds"
        | (x, y) -> Char.ToString(levelMap.Tiles.[y].[x])

    let rec mainLoop gameState =
        let command = getCommand()
        match command with
        | Quit -> ()
        | Help -> writeBox "Move: arrow keys Wait: . Quit: q" statusBar true
        | Wait -> writeBox "Waiting..." statusBar true
        | Move direction -> writeBox ("I wanna move " + string(direction).ToLower() + "!") statusBar true
        | Unknown -> writeBox "Unknown command, type ? for help." statusBar true
        if command <> Quit
            then mainLoop gameState

    [<EntryPoint>]
    let main argv =
        printMap levelMap
        writeAt startingGameState.Player.Position '@'
        writeBox "Ready." statusBar true
        mainLoop startingGameState
        writeBox ("Symbol at (3, 1): " + getCoord 3 1) statusBar false
        0 // return an integer exit code
