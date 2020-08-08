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
        Width = 7
        Height = 4
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
            | Quit -> writeAndPass gameState "Bye." statusBar false // assumes status bar is last line
            | Help -> writeAndPass gameState "Move: arrow keys Wait: . Quit: q" statusBar true
            | Wait -> writeAndPass gameState "Waiting..." statusBar true
            | Move direction -> moveAction gameState direction
            | Unknown -> writeAndPass gameState "Unknown command, type ? for help." statusBar true
        if command <> Quit
            then mainLoop newGameState

    [<EntryPoint>]
    let main argv =
        printMap levelMap
        writeAt startingGameState.Player.Position '@'
        writeBox "Ready." statusBar true
        mainLoop startingGameState
        0 // return an integer exit code
