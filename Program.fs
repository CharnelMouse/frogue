namespace Frogue
module Main =
    open System
    open Types
    open Input
    open Screen
    open Action

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
 
    let rec mainLoop gameState =
        let command = getCommand()
        let newGameState =
            match command with
            | Quit -> gameState
            | Help -> writeAndPass gameState "Move: arrow keys Wait: . Quit: q" statusBar
            | Wait -> writeAndPass gameState "Waiting..." statusBar
            | Move direction -> moveAction gameState direction
            | Unknown -> writeAndPass gameState "Unknown command, type ? for help." statusBar
        if command <> Quit
            then mainLoop newGameState

    [<EntryPoint>]
    let main argv =
        printMap levelMap
        writeAt startingGameState.Player.Position '@'
        writeBox "Ready." statusBar true
        mainLoop startingGameState
        writeBox ("Symbol at (3, 1): " + Char.ToString(getTileAt {X = 3; Y = 1} levelMap)) statusBar false
        0 // return an integer exit code
