namespace Frogue
module Main =
    open Types
    open Input
    open Frogue.Map
    open Output
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
        StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
        LastAction = CompleteAction StartSession
    }

    let rec mainLoop gameState =
        let command = getCommand gameState.LastAction
        let newGameState = resolveCommand gameState command
        updateOutput newGameState
        match newGameState.LastAction with
        | CompleteAction QuitAction -> ()
        | _ -> mainLoop newGameState

    [<EntryPoint>]
    let main argv =
        updateOutput startingGameState
        mainLoop startingGameState
        0 // return an integer exit code
