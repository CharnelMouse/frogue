namespace Frogue
module Main =
    open Types
    open Input
    open Frogue.Map
    open Output
    open Action

    let levelMap = createMap 7 4 [
            "#######"
            "#  +   "
            "#  #  #"
            "#######"
        ]

    let startingGameState = {
        Player = {Position = {X = 1; Y = 1}}
        Map = levelMap
        StatusBar = {Start = {X = 0; Y = 6}; Length = 35}
    }

    let rec mainLoop gameState =
        let command = getCommand()
        let newGameState =
            resolveCommand gameState command
        if command <> Quit
            then mainLoop newGameState

    [<EntryPoint>]
    let main argv =
        printMap levelMap
        writeAt startingGameState.Player.Position '@'
        writeBox "Ready." startingGameState.StatusBar true
        mainLoop startingGameState
        0 // return an integer exit code
