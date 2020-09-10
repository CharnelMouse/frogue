namespace Frogue
module Main =
    open Types
    open Frogue.Map
    open Output
    open SaveSystem
    open Action
    open TimeSystem
    open ActionGenerator

    let private levelMap = createMap 20 10 (importTiles [
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
            {
                Position = {X = 1; Y = 1}
                Tile = PlayerTile
                Controller = Player
            }
            {
                Position = {X = 7; Y = 6}
                Tile = OrcTile
                Controller = AI
            }
        ]
        Map = levelMap
        StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 50}
        StatusBuffer = ""
        Action = CompleteAction StartSession
        Tileset = DefaultTileset
    }

    let rec private mainLoop gameState =
        let postTime =
            generateAction gameState
            |> executeAction
            |> updateOutput
            |> updateTime
        match postTime.Action with
        | CompleteAction QuitAction -> popStatus false postTime |> ignore
        | _ -> popStatus true postTime |> mainLoop

    [<EntryPoint>]
    let private main argv =
        let gameState =
            if saveGameExists "save.sav"
                then loadGame "save.sav"
                else startingGameState
        updateOutput gameState
        |> popStatus true
        |> mainLoop
        0 // return an integer exit code
