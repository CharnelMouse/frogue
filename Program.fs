namespace Frogue
module Main =
    open Types
    open Output
    open SaveSystem
    open Action
    open TimeSystem
    open ActionGenerator

    let private levelMap = Map.create 20 10 (importMapTiles [
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
                Name = "adventurer"
                Script = WaitScript
            }
            {
                Position = {X = 7; Y = 6}
                Tile = OrcTile
                Controller = AIController
                Name = "orc"
                Script = DumbHunt
            }
            {
                Position = {X = 8; Y = 5}
                Tile = OrcTile
                Controller = AIController
                Name = "other orc"
                Script = DumbHunt
            }
        ]
        Map = levelMap
        StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 70}
        StatusBuffer = {Receiver = Player; Stream = ""}
        Action = CompletePlayerAction StartSession
        Tileset = DefaultTileset
    }

    let rec private mainLoop gameState =
        let postTime =
            generateAction gameState
            |> executeAction
            |> updateOutput
            |> updateTime
        match postTime.Action with
        | CompletePlayerAction QuitAction -> popStatus false postTime |> ignore
        | _ -> popStatusIfReceiverTurn true postTime |> mainLoop

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
