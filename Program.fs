namespace Frogue
module Main =
    open Types
    open Input
    open Frogue.Map
    open Output
    open SaveSystem
    open Action

    let levelMap = createMap 20 10 [
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
    ]

    let startingGameState = {
        Player = {Position = {X = 1; Y = 1}}
        Map = levelMap
        StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 50}
        Action = CompleteAction StartSession
        Tileset = DefaultTileset
    }

    let rec mainLoop gameState =
        let command = getCommand gameState.Action
        let newGameState = resolveCommand gameState command
        updateOutput newGameState
        match newGameState.Action with
        | CompleteAction QuitAction -> ()
        | _ -> mainLoop newGameState

    [<EntryPoint>]
    let main argv =
        let gameState =
            if saveGameExists ()
                then loadGame()
                else startingGameState
        updateOutput gameState
        mainLoop gameState
        0 // return an integer exit code
