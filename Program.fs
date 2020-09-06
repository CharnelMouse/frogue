namespace Frogue
module Main =
    open Types
    open Input
    open Tilesets
    open Frogue.Map
    open Output
    open CommandParser
    open SaveSystem
    open Action
    open TimeSystem

    let private levelMap = createMap 20 10 (convertTextTilesToTiles [
        "####################"
        "#              +    "
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "#              #   #"
        "####################"
    ])

    let private startingGameState = {
        Actors = [
            {Position = {X = 1; Y = 1}; Tile = PlayerTile}
            {Position = {X = 7; Y = 6}; Tile = OrcTile}
            ]
        Map = levelMap
        StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 50}
        Action = CompleteAction StartSession
        Tileset = DefaultTileset
    }

    let rec private mainLoop gameState =
        let command = getCommand gameState.Action
        let newGameState =
            resolveCommand gameState command
            |> executeAction
        updateOutput newGameState
        let timedGameState = updateTime newGameState 
        match timedGameState.Action with
        | CompleteAction QuitAction -> ()
        | _ -> mainLoop timedGameState

    [<EntryPoint>]
    let private main argv =
        let gameState =
            if saveGameExists ()
                then loadGame()
                else startingGameState
        updateOutput gameState
        mainLoop gameState
        0 // return an integer exit code
