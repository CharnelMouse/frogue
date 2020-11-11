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
        WorldState = {
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
                    Name = "orc"
                    Script = DumbHunt
                }
                {
                    Position = {X = 17; Y = 7}
                    Tile = OrcTile
                    Controller = AIController
                    Name = "orc"
                    Script = DumbHunt
                }
                {
                    Position = {X = 16; Y = 8}
                    Tile = OrcTile
                    Controller = AIController
                    Name = "orc"
                    Script = DumbHunt
                }
            ]
            Map = levelMap
            Action = CompletePlayerAction StartSession
        }
        OutputState = {
            StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
            StatusBuffer = {Receiver = Player; Stream = ""}
            Tileset = DefaultTileset
        }
    }

    let rec private mainLoop gameState =
        let postAction = {gameState with WorldState = generateAction gameState.WorldState}
        let postExecute =
            postAction
            |> executeAction
        let postOutput = {postExecute with OutputState = updateOutput postExecute}
        let postTime = {postOutput with WorldState = updateTime postOutput.WorldState}
        match postTime.WorldState.Action with
        | CompletePlayerAction QuitAction -> popStatus false true postTime.OutputState |> ignore
        | _ -> {postTime with OutputState = popStatusIfReceiverTurnOrFullLineInBuffer true postTime} |> mainLoop

    [<EntryPoint>]
    let private main argv =
        let gameState =
            if saveGameExists "save.sav"
                then loadGame "save.sav"
                else startingGameState
        let startState =
            {gameState with
                OutputState =
                    updateOutput gameState
                    |> popStatus true true
            }
        mainLoop startState
        0 // return an integer exit code
