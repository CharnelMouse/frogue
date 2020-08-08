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

    let startingGameState = {
        Player = {Position = {X = 1; Y = 1}}
        Map = levelMap
        StatusBar = {Start = {X = 0; Y = 6}; Length = 35}
    }
 
    let rec mainLoop gameState =
        let command = getCommand()
        let newGameState =
            match command with
            | Quit -> writeStatusAndPass gameState "Bye." false // assumes status bar is last line
            | Help -> writeStatusAndPass gameState "Move: arrow keys Wait: . Quit: q" true
            | Wait -> writeStatusAndPass gameState "Waiting..." true
            | Move direction -> moveAction gameState direction
            | Unknown -> writeStatusAndPass gameState "Unknown command, type ? for help." true
        if command <> Quit
            then mainLoop newGameState

    [<EntryPoint>]
    let main argv =
        printMap levelMap
        writeAt startingGameState.Player.Position '@'
        writeBox "Ready." startingGameState.StatusBar true
        mainLoop startingGameState
        0 // return an integer exit code
