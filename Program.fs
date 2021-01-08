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

    let private startingWorldState = {
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
    }

    let private startingOutputState = {
        StatusBar = {Start = {X = 0; Y = levelMap.Height + 1}; Length = 40}
        StatusBuffer = {Receiver = Player; Stream = ""}
        Tileset = DefaultTileset
    }

    let private startingAction = CompletePlayerAction StartSession

    let rec private mainLoop worldState outputState action =
        let newAction = generateAction worldState action
        let postExecuteWorld = executeAction worldState newAction
        let postOutputOutput = updateOutput postExecuteWorld outputState newAction
        let postTimeWorld = updateTime postExecuteWorld newAction
        let anyPlayerPresent = List.tryFind (fun a -> a.Controller = Player) postTimeWorld.Actors
        match anyPlayerPresent, newAction with
        | None, _ -> postOutputOutput |> pushDieMessage |> popStatus false false |> ignore
        | Some _, CompletePlayerAction QuitAction -> popStatus false true postOutputOutput |> ignore
        | _ ->
            let newOutput =
                postOutputOutput
                |> popStatusIfReceiverTurnOrFullLineInBuffer true postTimeWorld
            mainLoop postTimeWorld newOutput newAction

    [<EntryPoint>]
    let private main argv =
        let worldState, outputState, action =
            if saveGameExists "save.sav"
                then loadGame "save.sav"
                else startingWorldState, startingOutputState, startingAction
        let startOutput =
            updateOutput worldState outputState action
            |> popStatus true true
        mainLoop worldState startOutput action
        System.Console.ReadLine() |> ignore
        0 // return an integer exit code
